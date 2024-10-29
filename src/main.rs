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
    io::{self, Write}, ptr::null_mut,
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
    context: *mut Context,
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
    Builtin,
}

impl Tag {
    fn is_callable(&self) -> bool {
        matches!(self, Tag::Func | Tag::Builtin)
    }
}

type BuiltinFn = *const (dyn Fn(&mut Evaluator, *const Object, *mut Context) -> Result<(), String> + 'static);

#[repr(C)]
#[derive(Clone, Copy)]
union Data {
    int: i64,
    string: *const String,
    symbol: *const String,
    list: Cell,
    func: Func,
    builtin: BuiltinFn,
}

impl std::fmt::Debug for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(
                f,
                "Data {{ int: {:?}, string: {:?}, symbol: {:?}, list: {:?}, func: {:?} }}",
                self.int, self.string, self.symbol, self.list, self.func
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
}

#[repr(C)]
#[derive(Debug, Clone)]
struct Context {
    symbols: HashMap<String, *const Object>,
    parent: *mut Context,
}

struct Memory {
    heap: *mut Object,
    limit: *mut Object,
    next: *mut Object,
    string_pool: Vec<String>, // TODO improve
    string_pool_limit: usize,
    context: *mut Context,
    context_limit: *mut Context,
    next_context: *mut Context,
}

impl Memory {
    fn from_ptr(ptr: *mut Object, context: *mut Context, size: usize) -> Self {
        // if cap change -> dangling ptr
        let string_pool_limit = 1024;
        let string_pool = Vec::with_capacity(string_pool_limit);
        Self {
            heap: ptr,
            limit: unsafe { (ptr as *const u8).add(size) as *mut Object },
            next: ptr,
            string_pool,
            string_pool_limit,
            context,
            context_limit: unsafe { (context as *const u8).add(size) as *mut Context },
            next_context: context,
        }
    }

    fn alloc(&mut self) -> *mut Object {
        if unsafe { self.next.add(1) } >= self.limit {
            panic!("Out of memory");
        }
        let ptr = self.next;
        self.next = unsafe { self.next.add(1) };
        ptr
    }

    fn alloc_context(&mut self) -> *mut Context {
        if unsafe { self.next_context.add(1) } >= self.context_limit {
            panic!("Out of memory");
        }
        let ptr = self.next_context;
        unsafe {
            (*ptr).parent = null_mut();
            (*ptr).symbols = HashMap::new();
        }
        self.next_context = unsafe { self.next_context.add(1) };
        ptr
    }

    fn intern(&mut self, s: String) -> *const String {
        if self.string_pool.len() >= self.string_pool_limit {
            panic!("String pool limit exceeded");
        }
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
    stack: Vec<*const Object>,                // TODO improve
    root_context: *mut Context,
    memory: Memory,
}

impl Evaluator {
    const NIL_OBJ: Object = Object {
        tag: Tag::Nil,
        context: null_mut(),
        data: Data { int: 0 },
    };
    const NIL: *const Object = &Self::NIL_OBJ;

    fn new(memory: Memory) -> Self {
        Self {
            stack: Vec::new(),
            root_context: null_mut(),
            memory,
        }
    }

    fn init(&mut self) {
        self.root_context = self.memory.alloc_context();

        self.register_builtin("quote", &Self::eval_quote as BuiltinFn);
        self.register_builtin("define", &Self::eval_define as BuiltinFn);
        self.register_builtin("print", &Self::eval_print as BuiltinFn);
        self.register_builtin("+", &Self::eval_add as BuiltinFn);
        self.register_builtin("if", &Self::eval_if as BuiltinFn);
        self.register_builtin("<", &Self::eval_lt as BuiltinFn);
    }

    fn register_builtin(&mut self, symbol: &str, func: BuiltinFn) {
        let ptr = self.memory.alloc();
        unsafe {
            (*ptr).tag = Tag::Builtin;
            (*ptr).data.builtin = func;
        }
        unsafe {
            (*self.root_context).symbols.insert(symbol.to_string(), ptr);
        }
    }

    fn eval(&mut self, ast: &AST) -> Result<(), String> {
        let obj = self.alloc_ast(ast)?;
        self.do_eval(obj, self.root_context)?;
        Ok(())
    }

    fn eval_repl(&mut self, ast: &AST) -> Result<*const Object, String> {
        self.eval(ast)?;
        let result = self.stack.pop().unwrap();
        Ok(result)
    }

    fn do_eval(&mut self, obj: *const Object, context: *mut Context) -> Result<(), String> {
        let tag = unsafe { &(*obj).tag };
        match tag {
            Tag::Nil => self.eval_nil(obj)?,
            Tag::Int => self.eval_int(obj)?,
            Tag::String => self.eval_string(obj)?,
            Tag::Symbol => self.eval_symbol(obj, context)?,
            Tag::List => self.eval_fn_call(obj, context)?,
            Tag::Func => self.eval_func(obj)?,
            Tag::Builtin => self.eval_builtin(obj)?,
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

    fn eval_symbol(&mut self, obj: *const Object, context: *mut Context) -> Result<(), String> {
        let symbol = unsafe { (*obj).data.symbol };
        let symbol_str = unsafe { (*symbol).as_str() };

        let mut context = context;
        while context != null_mut() {
            let value = unsafe { (*context).symbols.get(symbol_str) };
            if value.is_some() {
                self.stack.push(*value.unwrap());
                return Ok(());
            }
            context = unsafe { (*context).parent };
        }
        Err(format!("symbol not found: {}", symbol_str))
    }

    fn eval_fn_call(&mut self, obj: *const Object, context: *mut Context) -> Result<(), String> {
        let func = unsafe { (*obj).data.list.head };
        let args = unsafe { (*obj).data.list.tail };
        self.do_eval(func, context)?;
        let func_obj = self.stack.pop().unwrap();
        let func_tag = unsafe { (*func_obj).tag };
        if !func_tag.is_callable() {
            return Err(format!("expected function or builtin, got {:?}", func_tag));
        }
        let args_tag = unsafe { (*args).tag };
        if args_tag != Tag::List {
            return Err(format!("expected arg list, got {:?}", args_tag));
        }
        if Tag::Builtin == func_tag {
            return self.eval_builtin_call(func_obj, args, context);
        }

        // there is no SP yet; calc diff
        let stack_len_before = self.stack.len();
        self.eval_list_spread(args, context)?;
        let args_len = self.stack.len() - stack_len_before;

        let mut signature = unsafe { (*func_obj).data.func.params };
        let mut new_env: HashMap<String, *const Object> = HashMap::new();

        for i in 0..args_len {
            let param = unsafe { (*signature).data.list.head };
            let param_tag = unsafe { (*param).tag };
            if param_tag != Tag::Symbol {
                return Err(format!("expected symbol, got {:?}", param_tag));
            }
            let value = self.stack[self.stack.len() - i - 1];
            let param_str = unsafe { (*param).data.symbol };
            new_env.insert(unsafe { (*param_str).clone() }, value);
            signature = unsafe { (*signature).data.list.tail };
        }
        for _ in 0..args_len {
            self.stack.pop();
        }
        let prev_context = unsafe { (*func_obj).context };
        let new_context = self.memory.alloc_context();
        unsafe {
            (*new_context).parent = prev_context;
            (*new_context).symbols = new_env;
        }
        self.do_eval(unsafe { (*func_obj).data.func.body }, new_context)?;
        let result_obj = self.stack.pop().unwrap();
        let new_result_obj = self.memory.alloc();
        unsafe {
            (*new_result_obj).tag = (*result_obj).tag;
            (*new_result_obj).context = new_context;
            (*new_result_obj).data.func = (*result_obj).data.func;
        }
        self.stack.push(new_result_obj);
        Ok(())
    }

    fn eval_builtin_call(
        &mut self,
        builtin: *const Object,
        args: *const Object,
        context: *mut Context,
    ) -> Result<(), String> {
        let func = unsafe { (*builtin).data.builtin };
        unsafe { (*func)(self, args, context) }
    }

    fn eval_builtin(&mut self, builtin: *const Object) -> Result<(), String> {
        self.stack.push(builtin);
        Ok(())
    }

    fn eval_quote(&mut self, args: *const Object, context: *mut Context) -> Result<(), String> {
        self.eval_list(args, context)?;
        let list = self.stack.pop().unwrap();
        self.stack.push(list);
        Ok(())
    }

    fn eval_define(&mut self, args: *const Object, context: *mut Context) -> Result<(), String> {
        let symbol = unsafe { (*args).data.list.head };
        let value = unsafe { (*(*args).data.list.tail).data.list.head };
        let symbol_str = unsafe { (*symbol).data.symbol };
        self.do_eval(value, context)?;
        let value = self.stack.pop().unwrap();

        let context = unsafe { (*symbol).context };
        unsafe {
            (*context).symbols.insert((*symbol_str).clone(), value);
        }
        self.stack.push(Self::NIL);
        Ok(())
    }

    fn eval_print(&mut self, args: *const Object, context: *mut Context) -> Result<(), String> {
        self.eval_list_spread(args, context)?;
        let arg = self.stack.pop().unwrap();
        self.do_eval_print(arg, context)?;
        println!();
        self.stack.push(Self::NIL);
        Ok(())
    }

    fn eval_add(&mut self, args: *const Object, context: *mut Context) -> Result<(), String> {
        let before_stack_len = self.stack.len();
        self.eval_list_spread(args, context)?;
        let arg_count = self.stack.len() - before_stack_len;

        let mut result = 0;
        for _ in 0..arg_count {
            let arg = self.stack.pop().unwrap();
            result += unsafe { (*arg).data.int };
        }
        let ptr = self.alloc_int(&result)?;
        self.stack.push(ptr);
        Ok(())
    }

    fn eval_if(&mut self, args: *const Object, context: *mut Context) -> Result<(), String> {
        let cond = unsafe { (*args).data.list.head };
        let then_branch = unsafe { (*(*args).data.list.tail).data.list.head };
        let else_branch = unsafe { (*(*(*args).data.list.tail).data.list.tail).data.list.head };

        self.do_eval(cond, context)?;
        let cond = self.stack.pop().unwrap();
        let cond_tag = unsafe { (*cond).tag };
        if cond_tag != Tag::Int {
            return Err(format!("expected int, got {:?}", cond_tag));
        }
        if unsafe { (*cond).data.int } != 0 {
            self.do_eval(then_branch, context)?;
        } else {
            self.do_eval(else_branch, context)?;
        }
        Ok(())
    }

    fn eval_lt(&mut self, args: *const Object, context: *mut Context) -> Result<(), String> {
        self.eval_list_spread(args, context)?;
        let arg2 = self.stack.pop().unwrap();
        let arg1 = self.stack.pop().unwrap();
        let arg1_tag = unsafe { (*arg1).tag };
        let arg2_tag = unsafe { (*arg2).tag };
        if arg1_tag != Tag::Int || arg2_tag != Tag::Int {
            return Err(format!("expected int, got {:?} and {:?}", arg1_tag, arg2_tag));
        }
        let result = unsafe { (*arg1).data.int } < unsafe { (*arg2).data.int };
        let result = if result { 1 } else { 0 };
        let ptr = self.alloc_int(&result)?;
        self.stack.push(ptr);
        Ok(())
    }

    fn do_eval_print(&mut self, arg: *const Object, context: *mut Context) -> Result<(), String> {
        unsafe {
            match (*arg).tag {
                Tag::Int => print!("{}", (*arg).data.int),
                Tag::String => print!("{}", (*(*arg).data.string).as_str()),
                Tag::Nil => print!("nil"),
                Tag::Symbol => print!("{}", (*(*arg).data.symbol).as_str()),
                Tag::List => {
                    print!("(");
                    let mut list = arg;
                    loop {
                        let head = (*list).data.list.head;
                        self.do_eval_print(head, context)?;
                        list = (*list).data.list.tail;
                        if (*list).tag == Tag::Nil {
                            break;
                        }
                        print!(" ");
                    }
                    print!(")");
                }
                Tag::Func => print!("#<function>"),
                Tag::Builtin => print!("#<builtin>"),
            }
        }
        Ok(())
    }

    fn eval_list(&mut self, obj: *const Object, context: *mut Context) -> Result<(), String> {
        let mut list = obj;
        let mut new_list = self.memory.alloc();
        unsafe {
            (*new_list).tag = Tag::Nil;
            (*new_list).data.int = 0;
        }
        while unsafe { (*list).tag != Tag::Nil } {
            let head = unsafe { (*list).data.list.head };
            self.do_eval(head, context)?;
            list = unsafe { (*list).data.list.tail };

            let tail = new_list;
            new_list = self.memory.alloc();
            unsafe {
                (*new_list).tag = Tag::List;
                (*new_list).data.list = Cell { head, tail };
            }
        }
        self.stack.push(new_list);
        Ok(())
    }

    fn eval_list_spread(&mut self, obj: *const Object, context: *mut Context) -> Result<(), String> {
        let mut list = obj;
        while unsafe { (*list).tag != Tag::Nil } {
            let head = unsafe { (*list).data.list.head };
            self.do_eval(head, context)?;
            list = unsafe { (*list).data.list.tail };
        }
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
        Ok(Self::NIL)
    }

    fn alloc_int(&mut self, i: &i64) -> Result<*const Object, String> {
        let ptr = self.memory.alloc();
        unsafe {
            (*ptr).tag = Tag::Int;
            (*ptr).context = self.root_context;
            (*ptr).data.int = *i;
        }
        Ok(ptr)
    }

    fn alloc_string(&mut self, s: &String) -> Result<*const Object, String> {
        let str = self.memory.intern(s.clone());
        let ptr = self.memory.alloc();
        unsafe {
            (*ptr).tag = Tag::String;
            (*ptr).context = self.root_context;
            (*ptr).data.string = str;
        }
        Ok(ptr)
    }

    fn alloc_symbol(&mut self, s: &String) -> Result<*const Object, String> {
        let str = self.memory.intern(s.clone());
        let ptr = self.memory.alloc();
        unsafe {
            (*ptr).tag = Tag::Symbol;
            (*ptr).context = self.root_context;
            (*ptr).data.symbol = str;
        }
        Ok(ptr)
    }

    fn alloc_list(&mut self, elements: Vec<*const Object>) -> Result<*const Object, String> {
        let mut last = self.memory.alloc();
        unsafe {
            (*last).tag = Tag::Nil;
            (*last).context = self.root_context;
            (*last).data.int = 0;
        }

        for i in (0..elements.len()).rev() {
            let obj = elements[i];
            let cell = Cell {
                head: obj,
                tail: last,
            };
            last = self.memory.alloc();
            unsafe {
                (*last).tag = Tag::List;
                (*last).context = self.root_context;
                (*last).data.list = cell;
            }
        }
        Ok(last)
    }

    fn alloc_func(&mut self, params: &Box<AST>, body: &Box<AST>) -> Result<*const Object, String> {
        let ptr = self.memory.alloc();
        let params_ptr = self.alloc_ast(params)?;
        let body_ptr = self.alloc_ast(body)?;
        if let AST::List(_) = params.as_ref() {
            unsafe {
                (*ptr).tag = Tag::Func;
                (*ptr).context = self.root_context;
                (*ptr).data.func = Func {
                    params: params_ptr,
                    body: body_ptr,
                };
            }
            Ok(ptr)
        } else {
            Err(format!("expected param list, got {:?}", params))
        }
    }
}

fn main() {
    let size = 16384;
    let raw_memory = unsafe {
        let layout = std::alloc::Layout::from_size_align(size, 8).unwrap();
        std::alloc::alloc(layout) as *mut Object
    };
    let context = unsafe {
        let layout = std::alloc::Layout::from_size_align(size, 8).unwrap();
        std::alloc::alloc(layout) as *mut Context
    };
    let memory = Memory::from_ptr(raw_memory, context, size);
    let mut evaluator = Evaluator::new(memory);
    evaluator.init();

    println!("heap address: {:p}", raw_memory);
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
