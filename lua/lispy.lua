
-- lua -i lispy.lua
-- use '= var' in repl show value

fn = require "fn"

function prn(t) 
    print('start')
    for k, v in pairs(t) do
        print(k)
    end
end

function split(s, sep)
    if sep == nil then
        sep = "%s"
    end
    local t={} ; i=1
    for str in string.gmatch(s, "([^"..sep.."]+)") do
        t[i] = str
        i = i + 1
    end
    return t
end

function zip(ks, vs)
    assert(#ks == #vs)

    local t = {}
    for k, v in pairs(vs) do
        t[ks[k]] = v
    end
    return t
end

function merge_map(t1, t2)
    local t = {}
    for k, v in pairs(t1) do
        t[k] = v
    end
    for k, v in pairs(t2) do
        t[k] = v
    end
    return t
end

-- will destruct t1
function merge_map_(t1, t2)
    for k, v in pairs(t2) do
        t1[k] = v
    end
end

function copy(obj, seen)
  if type(obj) ~= 'table' then return obj end
  if seen and seen[obj] then return seen[obj] end
  local s = seen or {}
  local res = setmetatable({}, getmetatable(obj))
  s[obj] = res
  for k, v in pairs(obj) do res[copy(k, s)] = copy(v, s) end
  return res
end

-- http://lua-users.org/wiki/SimpleRound
function round(num, idp)
  local mult = 10^(idp or 0)
  return math.floor(num * mult + 0.5) / mult
end

function rest(arr)
    local ret = {}
    for i=2, #arr do
        ret[i-1] = arr[i]
    end
    return ret
end

function has_key(t, k)
    for _k, _ in pairs(t) do
        if _k == k then
            return true
        end
    end
    return false
end

--------

Symbol = function(s) return 'string' == type(s) end
List = function(t) return 'table' == type(t) end
Number = function(n) return 'number' == type(n) end

function parse(s)
    return read_from_tokens(tokenize(s))
end

function tokenize(s)
    local re = string.gsub
    local not_empty = function (s) return #s > 0 end
    return fn.filter(not_empty,
            split(re(re(s, '[(]', ' ( '), '[)]', ' ) '), ' '))
end

function read_from_tokens(tokens)
    assert(#tokens ~= 0, 'unexpected EOF while reading')
    local token = table.remove(tokens, 1)
    if '(' == token then
        local L = {}
        while tokens[1] ~= ')' do
            table.insert(L, read_from_tokens(tokens))
        end
        table.remove(tokens, 1)
        return L
    elseif ')' == token then
        error 'unexpected )'
    else
        return atom(token)
    end
end

function atom(token)
    local ret, res = pcall(function () return token + 0 end)
    if ret then
        return res
    else
        return token .. ''
    end
end

function standard_env()
    local env = env_()
    env = merge_map(env, math)
    return merge_map(env, {
        ['+'] = function(x, y) return x + y end,
        ['-'] = function(x, y) return x - y end,
        ['*'] = function(x, y) return x * y end,
        ['/'] = function(x, y) return x / y end,
        ['>'] = function(x, y) return x > y end,
        ['<'] = function(x, y) return x < y end,
        ['>='] = function(x, y) return x >= y end,
        ['<='] = function(x, y) return x <= y end,
        ['='] = function(x, y) return x == y end,
        ['append'] = function(x, y) return x + y end,
        ['apply'] =  function(x, args) return x(unpack(args)) end,
        -- ['begin'] =  lambda *x: x[-1],
        ['car'] =    function(x) return x[1] end,
        ['cdr'] =    function(x) return rest(x) end,
        ['cons'] =   function(x, y) return table.insert(copy(x), y) end,
        ['eq?'] =    function(x, y) return x == y end,
        ['equal?'] = function(x, y) return x == y end,
        ['length'] = function(x) return #x end,
        ['list'] =   function(x) if not List(x) then x = {x} end; return {unpack(x)} end,
        ['list?'] =  List,
        ['map'] =    fn.map,
        ['not'] =    function(x) return not x end,
        ['null?'] =  function(x) return nil == x end,
        ['number?'] = Number,
        ['procedure?'] = function(x) return 'function' == type(x) end,
        ['round'] =  round,
        ['symbol?'] = Symbol,
        -- ['time'] =  function(x) return x[1] end
    })
end

function env_(params, args, outer)
    params = params or {}
    args = args or {}
    outer = outer or nil
    local t = zip(params, args)
    t.outer = outer
    return t
end

function env_find(env, k)
    if has_key(env, k) then
        return env
    else
        local outer = env.outer
        if outer then
            return env_find(outer, k)
        else
            return env
        end
    end
end

global_env = standard_env()

function repl(prompt)
    prompt = prompt or 'lis.py> '
    while true do
        io.stdout:write(prompt)
        val = eval(parse(io.stdin:read()))
        if val then
            print(lispstr(val))
        end
    end
end

function lispstr(exp)
    if List(exp) then
        return '(' .. table.concat(fn.map(lispstr, exp), ' ') .. ')'
    else
        return exp
    end
end

function procedure(params, body, env)
    return function(...)
        local args = {...}
        return eval(body, env_(params, args, env))
    end
end

function eval(x, env)
    env = env or global_env
    if Symbol(x) then
        return env_find(env, x)[x]
    elseif not List(x) then
        return x
    else
        local x0 = x[1]
        if x0 == 'quote' then
            return rest(x)
        elseif x0 == 'if' then
            local test = x[2]
            local conseq = x[3]
            local alt = x[4]
            local exp
            if eval(test, env) then
                exp = conseq
            else
                exp = alt
            end
            return eval(exp, env)
        elseif x0 == 'define' then
            local var = x[2]
            local exp = x[3]
            env[var] = eval(exp, env)
        elseif x0 == 'set!' then
            local var = x[2]
            local exp = x[3]
            env_find(env, var)[var] = eval(exp, env)
        -- FIXME while exec lambda, can't pass nil or false
        elseif x0 == 'lambda' then
            local params = x[2]
            local body = x[3]
            return procedure(params, body, env)
        else
            local proc = eval(x[1], env)
            local args = fn.map(function(x) return eval(x, env) end, rest(x))
            return proc(unpack(args))
        end
    end
end

-- repl()

