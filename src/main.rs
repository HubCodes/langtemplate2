use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{char, multispace0},
    combinator::map,
    multi::many0,
    sequence::delimited,
    IResult,
};

use std::{
    collections::HashMap,
    io::{self, Write},
};

#[derive(Debug, Clone)]
enum AST {
    Nil,
    Int(i64),
    String(String),
    Symbol(String),
    List(Vec<AST>),
    Func(Box<AST>, Box<AST>),
}

#[repr(C)]
#[derive(Debug, Clone)]
struct Object {
    tag: Tag,
    data: Data,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Copy)]
enum Tag {
    Nil,
    Int,
    String,
    Symbol,
    List,
    Func,
}

#[repr(C)]
#[derive(Clone, Copy)]
union Data {
    int: i64,
    string: *const String,
    symbol: *const String,
    list: Cell,
    func: Func,
}

impl std::fmt::Debug for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(
                f,
                "Data {{ int: {:?}, string: {:?}, symbol: {:?}, list: {:?} }}",
                self.int, self.string, self.symbol, self.list
            )
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct Cell {
    head: *const Object,
    tail: *const Object,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct Func {
    params: *const Object,
    body: *const Object,
    arity: usize,
}

struct Memory {
    heap: *mut Object,
    string_pool: Vec<String>, // TODO improve
}

impl Memory {
    fn from_ptr(ptr: *mut Object) -> Self {
        Self {
            heap: ptr,
            string_pool: Vec::new(),
        }
    }

    fn alloc(&mut self) -> *mut Object {
        let ptr = self.heap;
        self.heap = unsafe { self.heap.add(1) };
        ptr
    }

    fn intern(&mut self, s: String) -> *const String {
        self.string_pool.push(s);
        self.string_pool.last().unwrap() as *const String
    }
}

struct Parser;

impl Parser {
    fn parse_atom(input: &str) -> IResult<&str, AST> {
        let (input, atom) = alt((
            |input| Self::parse_int(input),
            |input| Self::parse_string(input),
            |input| Self::parse_symbol(input),
        ))(input)?;
        Ok((input, atom))
    }

    fn parse_int(input: &str) -> IResult<&str, AST> {
        let (input, result) = map(take_while1(Self::is_digit_char), |s: &str| {
            AST::Int(s.parse::<i64>().unwrap())
        })(input)?;
        Ok((input, result))
    }

    fn parse_string(input: &str) -> IResult<&str, AST> {
        let (input, result) = map(
            delimited(char('"'), take_while1(|c| c != '"'), char('"')),
            |s: &str| AST::String(s.to_string()),
        )(input)?;
        Ok((input, result))
    }

    fn parse_symbol(input: &str) -> IResult<&str, AST> {
        let (input, result) = map(take_while1(Self::is_symbol), |s: &str| {
            AST::Symbol(s.to_string())
        })(input)?;
        Ok((input, result))
    }

    fn parse_list(input: &str) -> IResult<&str, AST> {
        let (input, elements) = delimited(
            char('('),
            many0(delimited(
                multispace0,
                |input| Self::parse_expr(input),
                multispace0,
            )),
            char(')'),
        )(input)?;
        match elements.as_slice() {
            // empty list
            [] => Ok((input, AST::Nil)),
            // function definition
            [AST::Symbol(s), AST::List(params), body] if s == "fn" => Ok((
                input,
                AST::Func(Box::new(AST::List(params.clone())), Box::new(body.clone())),
            )),
            // normal list
            _ => Ok((input, AST::List(elements))),
        }
    }

    fn parse_expr(input: &str) -> IResult<&str, AST> {
        alt((
            |input| Self::parse_atom(input),
            |input| Self::parse_list(input),
        ))(input)
    }

    fn parse_sexpr(input: &str) -> IResult<&str, AST> {
        delimited(multispace0, |input| Self::parse_expr(input), multispace0)(input)
    }

    fn is_digit_char(c: char) -> bool {
        c.is_digit(10)
    }

    fn is_symbol(c: char) -> bool {
        c.is_alphanumeric() || "+-*/<=>!?&$%".contains(c)
    }
}

struct Evaluator {
    stack: Vec<*const Object>,
    env: Vec<HashMap<String, *const Object>>,
    memory: Memory,
}

impl Evaluator {
    fn new(memory: Memory) -> Self {
        let global_env = HashMap::new();
        Self {
            stack: Vec::new(),
            env: vec![global_env],
            memory,
        }
    }

    fn eval(&mut self, ast: &AST) -> Result<(), String> {
        let obj = self.alloc_ast(ast)?;
        self.do_eval(obj)?;
        Ok(())
    }

    fn eval_repl(&mut self, ast: &AST) -> Result<*const Object, String> {
        self.eval(ast)?;
        let result = self.stack.pop().unwrap();
        Ok(result)
    }

    fn do_eval(&mut self, obj: *const Object) -> Result<(), String> {
        let tag = unsafe { &(*obj).tag };
        match tag {
            Tag::Nil => self.eval_nil(obj)?,
            Tag::Int => self.eval_int(obj)?,
            Tag::String => self.eval_string(obj)?,
            Tag::Symbol => self.eval_symbol(obj)?,
            Tag::List => self.eval_fn_call(obj)?,
            Tag::Func => self.eval_func(obj)?,
        }
        Ok(())
    }

    fn eval_nil(&mut self, obj: *const Object) -> Result<(), String> {
        self.stack.push(obj);
        Ok(())
    }

    fn eval_int(&mut self, obj: *const Object) -> Result<(), String> {
        self.stack.push(obj);
        Ok(())
    }

    fn eval_string(&mut self, obj: *const Object) -> Result<(), String> {
        self.stack.push(obj);
        Ok(())
    }

    fn eval_symbol(&mut self, obj: *const Object) -> Result<(), String> {
        let symbol = unsafe { (*obj).data.symbol };
        let env = self.env.last().unwrap();
        let symbol_str = unsafe { (*symbol).as_str() };
        let value = env
            .get(symbol_str)
            .ok_or(format!("symbol not found: {}", symbol_str))?;
        self.stack.push(*value);
        Ok(())
    }

    fn eval_fn_call(&mut self, obj: *const Object) -> Result<(), String> {
        let func = unsafe { (*obj).data.list.head };
        let args = unsafe { (*obj).data.list.tail };
        self.do_eval(func)?;
        let func_obj = self.stack.pop().unwrap();
        let func_tag = unsafe { (*func_obj).tag };
        if func_tag != Tag::Func {
            return Err(format!("expected function, got {:?}", func_tag));
        }
        let args_tag = unsafe { (*args).tag };
        if args_tag != Tag::List {
            return Err(format!("expected arg list, got {:?}", args_tag));
        }
        self.eval_list(args)?;
        self.stack.pop(); // drop nil

        let arity = unsafe { (*func_obj).data.func.arity };
        let mut signature = unsafe { (*func_obj).data.func.params };
        let mut new_env: HashMap<String, *const Object> = HashMap::new();

        for i in 0..arity {
            let param = unsafe { (*signature).data.list.head };
            let value = self.stack[self.stack.len() - i - 1];
            let param_str = unsafe { (*param).data.symbol };
            new_env.insert(unsafe { (*param_str).clone() }, value);
            signature = unsafe { (*signature).data.list.tail };
        }
        self.env.push(new_env);
        self.do_eval(unsafe { (*func_obj).data.func.body })?;
        self.env.pop();

        Ok(())
    }

    fn eval_list(&mut self, obj: *const Object) -> Result<(), String> {
        let list = unsafe { &(*obj).data.list };
        let head = list.head;
        let tail = list.tail;
        self.do_eval(head)?;
        self.do_eval(tail)?;
        Ok(())
    }

    fn eval_func(&mut self, obj: *const Object) -> Result<(), String> {
        self.stack.push(obj);
        Ok(())
    }

    fn alloc_ast(&mut self, ast: &AST) -> Result<*const Object, String> {
        match ast {
            AST::Nil => self.alloc_nil(),
            AST::Int(i) => self.alloc_int(i),
            AST::String(s) => self.alloc_string(s),
            AST::Symbol(s) => self.alloc_symbol(s),
            AST::List(l) => {
                let elements = l
                    .iter()
                    .map(|e| self.alloc_ast(e))
                    .collect::<Result<Vec<*const Object>, String>>()?;
                self.alloc_list(elements)
            }
            AST::Func(params, body) => self.alloc_func(params, body),
        }
    }

    fn alloc_nil(&mut self) -> Result<*const Object, String> {
        let ptr = self.memory.alloc();
        unsafe {
            ptr.write(Object {
                tag: Tag::Nil,
                data: Data { int: 0 },
            });
        }
        Ok(ptr)
    }

    fn alloc_int(&mut self, i: &i64) -> Result<*const Object, String> {
        let ptr = self.memory.alloc();
        unsafe {
            ptr.write(Object {
                tag: Tag::Int,
                data: Data { int: *i },
            });
        }
        Ok(ptr)
    }

    fn alloc_string(&mut self, s: &String) -> Result<*const Object, String> {
        let str = self.memory.intern(s.clone());
        let ptr = self.memory.alloc();
        unsafe {
            ptr.write(Object {
                tag: Tag::String,
                data: Data { string: str },
            });
        }
        Ok(ptr)
    }

    fn alloc_symbol(&mut self, s: &String) -> Result<*const Object, String> {
        let str = self.memory.intern(s.clone());
        let ptr = self.memory.alloc();
        unsafe {
            ptr.write(Object {
                tag: Tag::Symbol,
                data: Data { symbol: str },
            });
        }
        Ok(ptr)
    }

    fn alloc_list(&mut self, elements: Vec<*const Object>) -> Result<*const Object, String> {
        let mut last = self.memory.alloc();
        unsafe {
            last.write(Object {
                tag: Tag::Nil,
                data: Data { int: 0 },
            })
        }

        for i in (0..elements.len()).rev() {
            let obj = elements[i];
            let cell = Cell {
                head: obj,
                tail: last,
            };
            last = self.memory.alloc();
            unsafe {
                last.write(Object {
                    tag: Tag::List,
                    data: Data { list: cell },
                });
            }
        }
        Ok(last)
    }

    fn alloc_func(&mut self, params: &Box<AST>, body: &Box<AST>) -> Result<*const Object, String> {
        let ptr = self.memory.alloc();
        let params_ptr = self.alloc_ast(params)?;
        let body_ptr = self.alloc_ast(body)?;
        if let AST::List(l) = params.as_ref() {
            let arity = l.len();
            unsafe {
                ptr.write(Object {
                    tag: Tag::Func,
                    data: Data {
                        func: Func {
                            params: params_ptr,
                            body: body_ptr,
                            arity,
                        },
                    },
                });
            }
            Ok(ptr)
        } else {
            Err(format!("expected param list, got {:?}", params))
        }
    }
}

fn main() {
    let raw_memory = unsafe {
        let layout = std::alloc::Layout::from_size_align(1024, 8).unwrap();
        std::alloc::alloc(layout) as *mut Object
    };
    let memory = Memory::from_ptr(raw_memory);
    let mut evaluator = Evaluator::new(memory);

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        match Parser::parse_sexpr(&input.trim()) {
            Ok((_, result)) => {
                let obj = evaluator.eval_repl(&result);
                obj.and_then(|o| {
                    println!("{:?}", unsafe { &(*o) });
                    Ok(())
                })
                .unwrap_or_else(|e| println!("Error: {:?}", e));
            }
            Err(err) => {
                println!("Error parsing input: {:?}", err);
            }
        }
    }
}
