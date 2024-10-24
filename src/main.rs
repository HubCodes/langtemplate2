use std::{collections::HashMap, io::Write};

use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{char, multispace0},
    combinator::map,
    multi::many0,
    sequence::delimited,
    IResult,
};

#[derive(Debug, PartialEq, Clone)]
pub enum SExpr {
    Int(i64),
    String(String),
    Symbol(String),
    List(Vec<SExpr>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Ptr(usize);

#[derive(Debug, PartialEq, Clone, Copy)]
enum Prim {
    Nil,
    Ptr(Ptr),
    Int(i64),
}

#[derive(Debug, PartialEq, Clone)]
enum Object {
    Nil,
    String(String),
    List(Vec<Prim>),
    Closure(SExpr, SExpr),
}

struct Env {
    vars: HashMap<String, Prim>,
}

impl Env {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

struct Parser;

impl Parser {
    fn parse_atom(input: &str) -> IResult<&str, SExpr> {
        let (input, atom) = alt((
            map(
                take_while1(
                    Self::is_digit_char),
                    |s: &str| SExpr::Int(s.parse::<i64>().unwrap(),
                )
            ),
            map(
                delimited(
                    char('"'), 
                    take_while1(|c| c != '"'),
                    char('"')),
                    |s: &str| SExpr::String(s.to_string(),
                )
            ),
            map(
                take_while1(Self::is_symbol), 
                |s: &str| SExpr::Symbol(s.to_string()),
            ),
        ))(input)?;
        Ok((input, atom))
    }
    
    fn is_digit_char(c: char) -> bool {
        c.is_digit(10)
    }
    
    fn is_symbol(c: char) -> bool {
        c.is_alphanumeric() || "+-*/<=>!?&$%".contains(c)
    }
    
    fn parse_list(input: &str) -> IResult<&str, SExpr> {
        let (input, elements) = delimited(
            char('('),
            many0(delimited(multispace0, Self::parse_expr, multispace0)),
            char(')'),
        )(input)?;
        Ok((input, SExpr::List(elements)))
    }
    
    fn parse_expr(input: &str) -> IResult<&str, SExpr> {
        alt((Self::parse_atom, Self::parse_list))(input)
    }
    
    fn parse_sexpr(input: &str) -> IResult<&str, SExpr> {
        delimited(multispace0, Self::parse_expr, multispace0)(input)
    }
}

struct Interpreter {
    stack: Vec<Prim>,
    heap: Vec<Object>,
    alloc_ptr: usize,
    env: Vec<Env>,
}

impl Interpreter {
    const HALF_HEAP_SIZE: usize = 512;

    fn new() -> Self {
        let mut heap = vec![];
        heap.resize(Self::HALF_HEAP_SIZE, Object::Nil);
        Self { 
            stack: vec![], 
            heap,
            alloc_ptr: 0,
            env: vec![Env::new()],
        }
    }

    fn eval(&mut self, expr: &SExpr) -> Result<(), String> {
        match expr {
            SExpr::Int(value) => {
                self.push_int(*value)?;
            },
            SExpr::String(value) => {
                self.push_string(value)?;
            },
            SExpr::Symbol(value) => {
                self.push_symbol(value)?;
            },
            SExpr::List(vec) => {
                self.handle_operator(vec)?;
            },
        }
        Ok(())
    }

    fn eval_for_repl(&mut self, expr: &SExpr) -> Prim {
        self.eval(expr).unwrap();
        let result = self.stack.last();
        if let Some(result) = result {
            *result
        } else {
            Prim::Nil
        }
    }

    fn handle_operator(&mut self, vec: &Vec<SExpr>) -> Result<(), String> {
        let operator = vec.first().unwrap();
        match operator {
            SExpr::Symbol(value) => {
                match value.as_str() {
                    // (quote expr ...)
                    "quote" => {
                        self.handle_quote(vec)?;
                    },
                    // (if cond then else)
                    "if" => {
                        self.handle_if(vec)?;
                    },
                    "=" => {
                        self.handle_eq(vec)?;
                    },
                    "!=" => {
                        self.handle_neq(vec)?;
                    },
                    // (fn (args ...) body)
                    "fn" => {
                        self.create_closure(vec)?;
                    },
                    // TOS: no change
                    "def" => {
                        self.define_variable(vec)?;
                    },
                    // TOS: return value (int)
                    "+" => {
                        self.add_integers(&vec)?;
                    },
                    "-" => {
                        self.sub_integers(&vec)?;
                    },
                    "<=" => {
                        self.handle_leq(&vec)?;
                    },
                    // function call, TOS: return value
                    sym => {
                        self.call_function(sym, &vec)?;
                    },
                }
            },
            _ => return Err("unexpected error".to_string()),
        }
        Ok(())
    }

    fn handle_quote(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let tail = vec.iter().skip(1).collect::<Vec<_>>();
        let tail_len = tail.len();
        for ele in tail {
            self.eval(ele)?;
        }
        // stack: [..., first, ..., last] (eos)
        let mut list = vec![];
        for i in 0..tail_len {
            list.push(self.stack[self.stack.len() - i - 1]);
        }
        let list_object = Object::List(list);
        let ptr = self.alloc(list_object);
        self.stack.push(Prim::Ptr(ptr));
        Ok(())
    }

    fn handle_if(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let cond_expr = vec.get(1).ok_or_else(|| "expected condition".to_string())?;
        let then_expr = vec.get(2).ok_or_else(|| "expected then".to_string())?;
        let else_expr = vec.get(3).ok_or_else(|| "expected else".to_string())?;

        self.eval(cond_expr)?;
        let cond = self.stack.pop().unwrap();

        if let Prim::Int(value) = cond {
            if value == 0 {
                self.eval(else_expr)?;
            } else {
                self.eval(then_expr)?;
            }
        } else {
            return Err("expected int".to_string());
        }
        Ok(())
    }

    fn handle_eq(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let left_expr = vec.get(1).ok_or_else(|| "expected left".to_string())?;
        let right_expr = vec.get(2).ok_or_else(|| "expected right".to_string())?;

        self.eval(left_expr)?;
        self.eval(right_expr)?;

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        let value = match (left, right) {
            (Prim::Int(left), Prim::Int(right)) => left == right,
            (Prim::Ptr(left), Prim::Ptr(right)) => {
                let left_obj = &self.heap[left.0];
                let right_obj = &self.heap[right.0];
                left_obj == right_obj
            },
            _ => false,
        };
        let value = if value { 1 } else { 0 };
        self.stack.push(Prim::Int(value));

        Ok(())
    }

    fn handle_neq(&mut self, vec: &[SExpr]) -> Result<(), String> {
        self.handle_eq(vec)?;
        let value = self.stack.pop().unwrap();
        let value = if let Prim::Int(value) = value {
            if value == 0 { 1 } else { 0 }
        } else {
            0
        };
        self.stack.push(Prim::Int(value));
        Ok(())
    }

    fn alloc(&mut self, obj: Object) -> Ptr {
        self.heap[self.alloc_ptr] = obj;
        let ptr = Ptr(self.alloc_ptr);
        self.alloc_ptr += 1;
        ptr
    }

    fn push_int(&mut self, value: i64) -> Result<(), String> {
        self.stack.push(Prim::Int(value));
        Ok(())
    }

    fn push_string(&mut self, value: &String) -> Result<(), String> {
        let obj = self.alloc(Object::String(value.clone()));
        self.stack.push(Prim::Ptr(obj));
        Ok(())
    }

    fn push_symbol(&mut self, value: &String) -> Result<(), String> {
        let mut found = false;
        for ele in self.env.iter().rev() {
            if let Some(prim) = ele.vars.get(value) {
                found = true;
                self.stack.push(prim.clone());
                break;
            }
        }
        if !found {
            self.stack.push(Prim::Nil);
        }
        Ok(())
    }

    fn create_closure(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let args = &vec[1];
        if let SExpr::List(args) = args {
            for arg in args {
                if let SExpr::Symbol(_) = arg {
                    // checked, do nothing
                } else {
                    return Err("malformed arg".to_string());
                }
            }
        } else {
            return Err("malformed args".to_string());
        }
        let body = &vec[2];
        let closure = Object::Closure(args.clone(), body.clone());
        let ptr = self.alloc(closure);
        self.stack.push(Prim::Ptr(ptr));
        Ok(())
    }

    fn define_variable(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let symbol = &vec[1];
        if let SExpr::Symbol(symbol) = symbol {
            let expr = &vec[2];
            self.eval(expr)?;
            let tos = self.stack.pop().unwrap();
            self.env.last_mut().unwrap().vars.insert(symbol.clone(), tos);
        } else {
            return Err("expected pointer".to_string());
        }
        Ok(())
    }

    fn add_integers(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let tail = vec.iter().skip(1).collect::<Vec<_>>();
        let tail_len = tail.len();
        for ele in &tail {
            self.eval(ele)?;
        }
        let mut acc = 0;
        for _ in 0..tail_len {
            let tos = self.stack.pop().unwrap();
            if let Prim::Int(value) = tos {
                acc += value;
            } else {
                return Err("expected int".to_string());
            }
        }
        self.stack.push(Prim::Int(acc));
        Ok(())
    }

    fn sub_integers(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let tail = vec.iter().skip(1).collect::<Vec<_>>();
        let tail_len = tail.len();
        for ele in &tail {
            self.eval(ele)?;
        }
        let mut acc = 0;
        for i in 0..tail_len {
            let operand = self.stack[self.stack.len() - tail_len + i];
            if let Prim::Int(value) = operand {
                if i == 0 {
                    acc = value;
                } else {
                    acc -= value;
                }
            } else {
                return Err("expected int".to_string());
            }
        }
        self.stack.push(Prim::Int(acc));
        Ok(()) 
    }

    fn call_function(&mut self, sym: &str, vec: &[SExpr]) -> Result<(), String> {
        let mut deref = None;
        for env in self.env.iter().rev() {
            if let Some(value) = env.vars.get(sym) {
                deref = Some(value);
                break;
            }
        }

        let deref = deref.ok_or_else(|| format!("undefined symbol: {}", sym))?;
        match deref {
            Prim::Nil => Err(format!("undefined symbol: {}", sym)),
            Prim::Ptr(ptr) => {
                let obj = self.heap[ptr.0].clone(); // TODO: no clone
                self.handle_function_call(obj, vec)
            },
            Prim::Int(_) => Err(format!("expected function, but got int: {}", sym)),
        }
    }

    fn handle_function_call(&mut self, obj: Object, vec: &[SExpr]) -> Result<(), String> {
        match obj {
            Object::Closure(args, body) => {
                if let SExpr::List(args) = args {
                    self.evaluate_function_args(&args, vec)?;
                    let new_env = self.create_function_env(&args)?;
                    self.env.push(new_env);
                    self.eval(&body)?;
                    self.env.pop();
                }
                Ok(())
            },
            _ => Err(format!("expected function, but got: {:?}", obj)),
        }
    }

    fn evaluate_function_args(&mut self, args: &Vec<SExpr>, vec: &[SExpr]) -> Result<(), String> {
        let params = vec.iter().skip(1).collect::<Vec<_>>();
        if params.len() != args.len() {
            return Err(format!("expected {} args, but got {}", args.len(), params.len()));
        }
        for param in params {
            self.eval(param)?;
        }
        Ok(())
    }

    fn create_function_env(&mut self, args: &Vec<SExpr>) -> Result<Env, String> {
        let mut new_env = Env::new();
        for (i, arg) in args.iter().enumerate() {
            if let SExpr::Symbol(arg) = arg {
                new_env.vars.insert(arg.clone(), self.stack[self.stack.len() - args.len() + i]);
            } else {
                return Err(format!("malformed arg: {:?}", arg));
            }
        }
        Ok(new_env)
    }

    fn handle_leq(&mut self, vec: &[SExpr]) -> Result<(), String> {
        let left_expr = vec.get(1).ok_or_else(|| "expected left".to_string())?;
        let right_expr = vec.get(2).ok_or_else(|| "expected right".to_string())?;

        self.eval(left_expr)?;
        self.eval(right_expr)?;

        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        let value = match (left, right) {
            (Prim::Int(left), Prim::Int(right)) => left <= right,
            _ => return Err("expected integers for comparison".to_string()),
        };
        let value = if value { 1 } else { 0 };
        self.stack.push(Prim::Int(value));

        Ok(())
    }
}

fn main() {
    use std::io;

    let mut interpreter = Interpreter::new();
    loop {
        let mut input = String::new();
        print!("> ");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).expect("Failed to read line");
        let input = input.trim();
        if input.is_empty() {
            println!("to quit, press Ctrl-C");
            continue;
        }

        match Parser::parse_sexpr(input) {
            Ok((_, expr)) => {
                let result = interpreter.eval_for_repl(&expr);
                println!("{:?}", result);
            }
            Err(err) => println!("parse error: {:?}", err),
        }
    }
}
