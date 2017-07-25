
#![feature(slice_patterns)]

mod lispy {
    // use std::ops::*;
    use std::collections::HashMap;

    macro_rules! fns_op {
        (
            $op:tt, $t:ident
        ) => {
            LVal::F(|params| {
                match params {
                    LVal::SExpr(mut xs) => {
                        let x = xs.remove(0).to_float();
                        let y = xs.remove(0).to_float();
                        LVal::$t(x $op y)
                    },
                    _ => panic!("error!")
                }
            })
        }
    }

    macro_rules! fns_math {
        (
            $op:expr, $t:ident
        ) => {
            LVal::F(|params| {
                match params {
                    LVal::SExpr(mut xs) => {
                        let x = xs.remove(0).to_float();
                        let y = xs.remove(0).to_float();
                        LVal::$t($op(x, y))
                    },
                    _ => panic!("error!")
                }
            })
        }
    }

    pub fn abs(x: f64, _: f64) -> f64 {
        if x < 0.0 { -x } else { x }
    }

    pub fn min(x: f64, y: f64) -> f64 {
        if x > y { y } else { x }
    }

    // CAN'T use fn, or the closure can't convert to fn pointer as it closured var op 
    // pub fn fns_(op: &str) -> LVal {
    macro_rules! fns_ {
        (
            $op:expr
        ) => {
            LVal::F(|params| match params {
                LVal::SExpr(xs) => {
                    match $op {
                        "list" => LVal::SExpr(xs.clone()),
                        "car" => LVal::Sym(xs[0].clone().into_vec()[0].into_value()),
                        "cdr" => LVal::SExpr(xs[0].clone().into_vec()[1..].to_vec()),
                        "cons" => LVal::SExpr(xs.clone()),
                        "is_list" => LVal::Bool(true),
                        "is_eq" => LVal::Bool(xs[0] == xs[1]),
                        "len" => LVal::Num(xs.len() as f64),
                        "not" => LVal::Bool(
                            xs[0].into_value() == "false" 
                        ),
                        "is_null" => LVal::Bool(xs.len() == 0),
                        "is_number" => {
                            LVal::Bool(
                                if let Ok(_) = xs[0].into_value().parse::<f64>() {
                                    true
                                } else {
                                    false
                                }
                            )
                        },
                        "is_symbol" => {
                            LVal::Bool(
                                if let Ok(_) = xs[0].into_value().parse::<f64>() {
                                    false
                                } else {
                                    true
                                }
                            )
                        },
                        _ => panic!("error!"),
                    }
                }
                _ => panic!("error!"),
            })
        }
    }
   
    // https://internals.rust-lang.org/t/implement-fnmut-fn-for-box-fnmut-box-fn/2155
    /*
    pub fn fns_map() -> LVal {
        LVal::F(|params| match params {
            LVal::F(LVal::F(fns), mut xs) => LVal::expr(xs.iter().map(fns).collect()),
            _ => panic!("error!"),
        })
    }
    */
    
    type Symbol = String;

    #[derive(Debug)]
    #[derive(PartialEq, Clone)]
    pub struct Env {
        pub current: HashMap<String, LVal>,
        outer: Option<Box<Env>>,
    }

    impl Env {
        fn new(params: Vec<LVal>, mut args: Vec<LVal>, outer: Option<Box<Env>>) -> Env {
            let mut m = HashMap::new();

            for (i, v) in params.into_iter().enumerate() {
                if let LVal::Sym(v) = v {
                    m.insert(v, args.remove(i));
                }
            }

            Env {
                current: m,
                outer: outer,
            }
        }

        fn update(&mut self, m: HashMap<String, LVal>) {
            // let mut cm = &mut self.current;

            for (k, v) in m.into_iter() {
                self.current.insert(k, v);
            }
        }
        
        fn assoc(&mut self, k: String, v: LVal) {
            self.current.insert(k, v);
        }

        fn find(&self, var: &Symbol) -> &Env {
            if let Some(_) = self.current.get(var) {
                self
            } else {
                // try:
                // self.outer.unwrap().find(var)
                if let Some(outer) = self.outer.as_ref() {
                    outer.find(var)
                } else {
                    self
                }
            }
        }
    }

    pub trait StdFn {
        fn run(&self, params: LVal) -> LVal;
    }
   
    // https://stackoverflow.com/questions/36390665/in-rust-how-do-you-pass-a-function-as-a-parameter
    // Fn have to use Box 
    // type LValT = Fn(LVal) -> LVal;
    type LValT = fn(LVal) -> LVal;
    
    #[derive(Debug)]
    #[derive(PartialEq, Clone)]
    pub enum LVal {
        Num(f64),
        // Err(String),
        Sym(String),
        Str(String),
        Bool(bool),
        Proc {
            env: Env,
            params: Vec<LVal>,
            body: Vec<LVal>
        },
        F(LValT),
        SExpr(Vec<LVal>),
        Succ,
    }
    
    impl LVal {
        pub fn into_value(&self) -> String {
            if let &LVal::Sym(ref v) = self {
                v.clone()
            } else {
                panic!("not supported {:?}", self);
            }
        }
        
        pub fn into_vec(&self) -> Vec<LVal> {
            if let &LVal::SExpr(ref v) = self {
                v.clone()
            } else {
                panic!("not supported {:?}", self);
            }
        }
        
        pub fn to_float(&self) -> f64 {
            match self {
                &LVal::Sym(ref v) | &LVal::Str(ref v) => {
                    v.clone().parse::<f64>().unwrap()
                },
                &LVal::Num(v) => {
                    v
                },
                _ =>  panic!("not supported {:?}", self),
            }
        }
    }

    impl StdFn for LVal {
        fn run(&self, params: LVal) -> LVal {
            if let LVal::F(f) = *self {
                f(params)
            } else {
                panic!("no fn!")
            }
        }
    }
    
    pub fn eval(x: LVal, env: &mut Env) -> LVal {
        let mut _f = |f: LValT, x: &Vec<LVal>, env: &mut Env| {
            let mut args = vec![];
            for ref exp in x[1..].iter() {
                args.push(eval((*exp).clone(), env));
            }
            // can't collect
            // .collect();
            match f(LVal::SExpr(args)) {
            // if let LVal::F(v) = t {
                LVal::Num(v) => {
                    return LVal::Num(v)
                },
                LVal::Sym(v) => {
                    return LVal::Str(v)
                },
                LVal::Bool(v) => {
                    return LVal::Bool(v)
                },
                LVal::SExpr(v) => {
                    return LVal::SExpr(v)
                },
                v => panic!("eval type error 3 {:?}", v),
            }
        };
        
        match x {
            LVal::Sym(x) => {
                println!("x: {:?}", x);
                if let Some(v) = env.find(&x).current.get(&x) {
                    return v.clone()
                } else {
                    return LVal::Sym(x)
                }
            },
            
            LVal::Proc{..} => {
                return x
            },
    
            LVal::SExpr(x) => {
                if let LVal::Sym(ref car) = x[0] {
                    match car.as_ref() {
                        "quote" => {
                            return LVal::SExpr(x[1..].to_vec())
                        },
                        "if" => {
                            // pattern `[]` not covered?
                            // let [_, test, conseq, alt] = (*x.borrow())[..];
                            let xs = &x;
                            let test = &xs[1];
                            let conseq = &xs[2];
                            let alt = &xs[3];
                            if let LVal::Bool(pred) = eval(test.clone(), env) {
                                let exp = if pred {
                                    conseq
                                } else {
                                    alt
                                };
                                
                                return eval(exp.clone(), env)
                            } else {
                                panic!("eval type error 6 {:?}", x)
                            }
                        },
                        "define" => {
                            let xs = &x;
                            let var = &xs[1];
                            let exp = &xs[2];
                            if let &LVal::Sym(ref s) = var {
                                let expr = eval(exp.clone(), env);
                                env.assoc(s.clone(), expr);
                                return LVal::Succ
                            } else {
                                panic!("eval type error 5 {:?}", x)
                            }
                        },
                        "set!" => {
                            let xs = &x;
                            let var = &xs[1];
                            // let exp = &xs[2];
                            if let &LVal::Sym(_) = var {
                                // env.find(&s).current[&s].as_mut() = eval(exp, env);
                                return LVal::Succ
                            } else {
                                panic!("eval type error 4 {:?}", x)
                            }
                        },
                        "lambda" => {
                            let xs = &x;
                            if let &LVal::SExpr(ref params) = &xs[1] {
                                if let &LVal::SExpr(ref body) = &xs[2] {
                                    let procedure = LVal::Proc {
                                        params: params.clone(), 
                                        body: body.clone(), 
                                        env: env.clone()
                                    };
                                    return procedure
                                } else {
                                    panic!("eval type error 2a {:?}", x)
                                }
                            } else {
                                    panic!("eval type error 2a {:?}", x)
                            }
                        },
                        _ => {
                            if let LVal::F(f) = eval(x[0].clone(), env) {
                                return _f(f, &x, env)
                            } else {
                                panic!("eval type error 2 {:?}", x)
                            }
                        }
                    }
                } else if let LVal::F(f) = x[0] {
                    return _f(f, &x, env)
                } else if let LVal::Proc{ref params, ref body, ref env} = x[0] {
                    let args = x[1..].to_vec();
                    return eval(LVal::SExpr(body.clone()), &mut Env::new(params.clone(), args, Some(Box::new(env.clone()))))
                } else if let LVal::SExpr(_) = x[0] {
                    let mut xs = vec![];
                    xs.push(eval(x[0].clone(), env));
                    println!("xs {:?}, {:?}", x[0], xs);
                    for v in x[1..].to_vec().iter() {
                        xs.push(v.clone());
                    }
                    return eval(LVal::SExpr(xs), env)
                } else {
                    panic!("eval type error 1 {:?}", x)
                }
            },
            
            _ => panic!("eval type error 0 {:?}", x),
        }
    }

    pub fn parse(program: &str) -> LVal {
        let tokens = tokenize(program);
        read_from_tokens(&tokens).1
    }

    pub fn tokenize(s: &str) -> LVal {
        let syms = s.replace("(", " ( ")
                .replace(")", " ) ")
                .split_whitespace()
                .map(|x| LVal::Sym(String::from(x)))
                .collect();
        LVal::SExpr(syms)
    }

    pub fn read_from_tokens(_tokens: &LVal) -> (LVal, LVal) {
        let tokens0 = match _tokens {
            &LVal::SExpr(ref tokens) => {
                // tokens.borrow().clone()
                tokens.clone()
            },
            _ => panic!("unexpected type {:?}", _tokens),
        };
        
        let tokens = tokens0;
        if tokens.len() == 0 {
            panic!("unexpected EOF while reading");
        }

        let token = match tokens[0] {
            LVal::Sym(ref token) => token,
            _ => panic!("unexpected type"),
        };
        
        let mut tokens = tokens[1..].to_vec();

        if token == "(" {
            let mut xs = vec![];
            while true {
                let first = tokens[0].clone();
                let token = match first {
                    LVal::Sym(ref token) => token,
                    _ => panic!("unexpected type"),
                };

                if token == ")" {
                    break;
                }
               
                let tmp = read_from_tokens(&LVal::SExpr(tokens.clone()));
                let x = tmp.1;
                if let LVal::SExpr(ref t) = tmp.0 {
                    tokens = t.clone()
                };
                xs.push(x);
            }

            let tokens = tokens[1..].to_vec();
            return (LVal::SExpr(tokens), LVal::SExpr(xs));
        } else if token == ")" {
            panic!("unexpected )");
        } else {
            (LVal::SExpr(tokens), atom(token.clone()))
        }
    }

    pub fn atom(token: String) -> LVal {
        LVal::Sym(token)
    }

    pub fn standard_env() -> Env {
        let mut env = Env::new(vec![LVal::Sym(String::from("-"))], vec![fns_op!(-, Num)], None);
        let mut m = HashMap::new();
        let fns = vec![
            // ("+", Add::add),
            ("+", fns_op!(+, Num)),
            ("*", fns_op!(*, Num)),
            ("/", fns_op!(/, Num)),
            (">", fns_op!(>, Bool)),
            ("<", fns_op!(<, Bool)),
            (">=", fns_op!(>=, Bool)),
            ("<=", fns_op!(<=, Bool)),
            ("==", fns_op!(==, Bool)),
            ("!=", fns_op!(!=, Bool)),
            ("abs", fns_math!(abs, Num)),
            ("min", fns_math!(min, Num)),
            ("list", fns_!("list")),
            ("list?", fns_!("is_list")),
            ("car", fns_!("car")),
            ("cdr", fns_!("cdr")),
            ("cons", fns_!("cons")),
           
            ("eq?", fns_!("is_eq")),
            ("len", fns_!("len")),
            ("not", fns_!("not")),
            ("null?", fns_!("is_null")),
            ("number?", fns_!("is_number")),
            ("symbol?", fns_!("is_symbol")),
            // ("map", fns_map()),
        ];

        for (k, v) in fns.into_iter() {
            m.insert(String::from(k), v);
        }

        env.update(m);
        env
    }
}

fn main() {
    use lispy::*;
   
    // XXX: https://github.com/msiemens/mlisp-rs/blob/master/src/builtin/mod.rs
    println!("eval: {:?}", eval(parse("
    (+ 
        ((if (> 2 3) / *)
         (if (> 1 2) 100 10)
            (* 10 
                (car 
                    (cdr (list 1000 10000 10))))) 
        20)
    "), &mut standard_env()));
    
    println!("eval: {:?}", eval(parse("
    ((lambda (x) 
        (+ (* (/ x 7) 3) x)) 
    10)
    "), &mut standard_env()));
}

