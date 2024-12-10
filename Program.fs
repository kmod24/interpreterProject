module Program
open System

//dotnet fsi "/Users/kmod/Desktop/project-kmod24-main/Program.fs"

// This is a very basic math interpreter, using unions to discriminate between 
// expression operators.

// state represents the values in the map, which we can update accordingly

// An expression is either a constant value, or an arithmetic function applied to
// one or more operands that are expressions.
type Expression = 
    | Literal of float
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Neg of Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Sqrt of Expression
    | Pow of Expression * Expression
    | Input of string
    | Var of string
    | Call of string * Expression list

type Condition =
    | Equals of Expression * Expression
    | And of Condition * Condition
    | Or of Condition * Condition 
    | Not of Condition

type Statement =
    | Noop
    | PrintStr of string
    | PrintExp of Expression
    | Branch of Condition * Statement list * Statement list option
    | Repeat of int * Statement list
    | While of Condition * Statement list
    | Assign of string * Expression
    | Function of string * string list * Statement list //defines a function but does not execute it
    | Return of Expression //the return statement

// make a state record with two fields: 
// one map from string -> float for variables
// one from string to function definitions for functions

//defines a function, containing its parameters and its body
type FunctionDef = {
    Parameters: string list
    Body: Statement list
}

type ProgramState = {
    Variables: Map<string, float> //map to store variable names and their values (string to float)
    Functions: Map<string, FunctionDef> //map to store function names and their definitions (string to function definition)
}

let rec evaluate expr state =
    match expr with 
    | Literal c -> c
    | Var varName -> Map.find varName state.Variables
    | Add (left, right) -> (evaluate left state) + (evaluate right state)
    | Sub (left, right) -> (evaluate left state) - (evaluate right state)
    | Neg expr -> -(evaluate expr state)
    | Mul (left, right) -> (evaluate left state) * (evaluate right state)
    | Div (left, right) -> (evaluate left state) / (evaluate right state)
    | Sqrt expr -> Math.Sqrt(evaluate expr state)
    | Pow (baseExpr, exp) -> Math.Pow(evaluate baseExpr state, evaluate exp state)
    | Input prompt ->
        printf "%s" prompt
        let input = Console.ReadLine()
        match System.Double.TryParse(input) with //converts string input -> float 
        | (true, value) -> value //if successful, returns the parsed float value
        | _ -> failwith "Input is invalid!"
    | Call (funcName, args) ->
        let funcDef = Map.find funcName state.Functions //find the functions to use
        let argValues = List.map (fun arg -> evaluate arg state) args //evaluates each argument, producing a list of arg values
        let initialVariables = Map.ofList (List.zip funcDef.Parameters argValues)
        let initialState = { Variables = initialVariables; Functions = state.Functions }
        let resultState = interpretProgram funcDef.Body initialState
        Map.find "_return" resultState.Variables

// Approach to Call of string * Expression list
// find the functions first. then evaluate each of the arguments using List.map
// construct an initial state for the function. each parameter becomes a new variable corresponding to its value 
// which is equal to evaluating the corresponding expression from the argument list

// List.zip = takes two lists and pairs them up with their corresponding elements into a list of tuples (pairing items side by side)    
// Map.ofList -> takes a list of key-value pairs and turns it into a map
// List.map -> evaluate each item in the list, or in this case, each of the arguments


and evaluateCondition cond state = 
    match cond with
    | Equals (left, right) -> (evaluate left state) = (evaluate right state)
    | And (cond1, cond2) -> (evaluateCondition cond1 state) && (evaluateCondition cond2 state)
    | Or (cond1, cond2) -> (evaluateCondition cond1 state) || (evaluateCondition cond2 state)
    | Not cond -> not(evaluateCondition cond state)


and interpret statement state =
    match statement with 
    | Noop -> state //returns the same state
    | PrintStr str ->
        printfn "%s" str
        state
    | PrintExp expr ->
        let value = evaluate expr state
        printfn "%f" value
        state
    | Assign (varName, expr) -> 
        let value = evaluate expr state
        { state with Variables = Map.add varName value state.Variables } //creates a new state, updating the variables
    | Branch (cond, thenBranch, elseBranch) ->
        if evaluateCondition cond state then
            interpretProgram thenBranch state
        else 
            match elseBranch with
            | None -> state
            | Some elseBranch -> interpretProgram elseBranch state
    | Repeat (n, body) ->
        let rec repeatHelper count currentState = 
            if count <= 0 then currentState 
            else 
                let newState = interpretProgram body currentState
                repeatHelper (count - 1) newState
        repeatHelper n state
    | While (cond, body) ->
        let rec whileHelper currentState = 
            if evaluateCondition cond currentState then //evaluate the condition, then interpret entirety of statement list if true
                let newState = interpretProgram body currentState
                whileHelper newState
            else 
                currentState //existing state 
        whileHelper state
    | Function (name, param, body) ->
        let funcDef = { Parameters = param; Body = body }
        { state with Functions = Map.add name funcDef state.Functions } //create a name, make the function, add it to the map
    | Return expr ->
        let returnValue = evaluate expr state
        { state with Variables = Map.add "_return" returnValue state.Variables } // fake variable in the func state


and interpretProgram program state = 
    match program with
    | [] -> state //returns current state if there aren't any more
    | head :: tail ->
        let newState = interpret head state //interpret the first statement
        interpretProgram tail newState //then interpret the remaining statements

// FINAL CHALLENGE: language transcompiler -> representing this program into PYTHON :)

let rec expressionToPython expr = 
    match expr with
    | Literal c -> sprintf "%g" c
    | Var varName -> varName
    | Add (left, right) -> sprintf "%s + %s" (expressionToPython left) (expressionToPython right)
    | Sub (left, right) -> sprintf "%s - %s" (expressionToPython left) (expressionToPython right)
    | Neg expr -> sprintf "-%s" (expressionToPython expr)
    | Mul (left, right) -> sprintf "%s * %s" (expressionToPython left) (expressionToPython right)
    | Div (left, right) -> sprintf "%s / %s" (expressionToPython left) (expressionToPython right)
    | Sqrt expr -> sprintf "math.sqrt(%s)" (expressionToPython expr)
    | Pow (baseExpr, exp) -> sprintf "%s ** %s" (expressionToPython baseExpr) (expressionToPython exp)
    | Input prompt -> sprintf "input('%s')" prompt
    | Call (funcName, args) ->
        let argsPython = args |> List.map expressionToPython |> String.concat ","
        sprintf "%s(%s)" funcName argsPython


let rec conditionToPython cond = 
    match cond with
    | Equals (left, right) -> sprintf "%s == %s" (expressionToPython left) (expressionToPython right)
    | And (cond1, cond2) -> sprintf "%s and %s" (conditionToPython cond1) (conditionToPython cond2)
    | Or (cond1, cond2) -> sprintf "%s or %s" (conditionToPython cond1) (conditionToPython cond2)
    | Not cond -> sprintf "not %s" (conditionToPython cond)

let rec statementToPython prefix stmt =
    match stmt with
    | Noop -> "" 
    | PrintStr str -> sprintf "%sprint('%s')" prefix str 
    | PrintExp expr -> sprintf "%sprint(%s)" prefix (expressionToPython expr) 
    | Assign (varName, expr) -> sprintf "%s%s = %s" prefix varName (expressionToPython expr) 
    | Branch (cond, thenBranch, elseBranch) ->
        let thenCode = thenBranch |> List.map (statementToPython (prefix + "    ")) |> String.concat "\n"
        let elseCode =
            match elseBranch with
            | None -> ""
            | Some elseStatements ->
                let elseBody = elseStatements |> List.map (statementToPython (prefix + "    ")) |> String.concat "\n"
                sprintf "%selse:\n%s" prefix elseBody
        sprintf "%sif %s:\n%s\n%s" prefix (conditionToPython cond) thenCode elseCode
    | Repeat (n, body) ->
        let bodyCode = body |> List.map (statementToPython (prefix + "    ")) |> String.concat "\n"
        sprintf "%sfor _ in range(%d):\n%s" prefix n bodyCode
    | While (cond, body) ->
        let bodyCode = body |> List.map (statementToPython (prefix + "    ")) |> String.concat "\n"
        sprintf "%swhile %s:\n%s" prefix (conditionToPython cond) bodyCode
    | Function (name, parameters, body) ->
        let parameterList = String.concat ", " parameters
        let bodyCode = body |> List.map (statementToPython (prefix + "    ")) |> String.concat "\n"
        sprintf "%sdef %s(%s):\n%s" prefix name parameterList bodyCode
    | Return expr -> sprintf "%sreturn %s" prefix (expressionToPython expr)


// Example Program for Function
let program = [
    Assign ("x", Literal 10.0);
    While (Not (Equals (Literal 0.0, Var "x")), [
        PrintExp (Var "x");
        Assign ("x", Sub (Var "x", Literal 1.0))
    ]);
    PrintStr "Done!"
]

let pythonCode = program |> List.map (statementToPython "") |> String.concat "\n"
printfn "%s" pythonCode


// Statement Lists

// let initialState = Map.ofList [("x", 4.0)]  // hard-code a starting state of x=4.
// let myProgram = [
// Assign ("x", Add (Literal 1, Var "x"));
// PrintExp (Var "x");
// Assign ("x", Literal 0);
// PrintExp (Var "x")
// ]
// interpretProgram myProgram initialState |> printfn "%O"


// Adding a while loop

// // Example Program
// let myProgram = [
//     Assign ("i", Literal 0.0);  // i = 0
//     While (Not (Equals (Literal 10.0, Var "i")), [
//         PrintExp (Var "i");                          // Print i
//         Assign ("i", Add (Var "i", Literal 1.0))     // i = i + 1
//     ]);
//     PrintExp (Mul (Literal 2.0, Var "i"))            // Print 2 * i
// ]

// let initialState = Map.ofList []
// interpretProgram myProgram initialState |> printfn "%A"


// Functions
// // Example Program
// let myProgram = [
//     Function ("squared", ["a"], [
//         Return (Mul (Var "a", Var "a"))
//     ]);
//     Assign ("x", Input "Please enter a float: ");
//     Assign ("result", Call ("squared", [Var "x"]));
//     PrintExp (Var "x");
//     PrintStr "squared is equal to ";
//     PrintExp (Var "result")
// ]

// let initialState = { Variables = Map.empty; Functions = Map.empty }
// interpretProgram myProgram initialState |> ignore



// FINAL CHALLENGE TEST CASES

// expressionToPython
// let expr = Add (Literal 5.0, Literal 3.0)
// let expr1 = Sqrt(Literal 64.0)
// let expr2 = Add (Literal 5.0, Mul (Var "x", Literal 3.0))
// printfn "%s" (expressionToPython expr)


// printfn "%s" (expressionToPython expr)
// printfn "%s" (expressionToPython expr1)
// printfn "%s" (expressionToPython expr2)


// conditionToPython
// let cond = And (Equals (Literal 5.0, Var "x"), Equals (Literal 0.0, Var "y"))
// printfn "%s" (conditionToPython cond)



// statementToPython
// let program = [
//     Assign ("x", Literal 10.0);
//     While (Not (Equals (Literal 0.0, Var "x")), [
//         PrintExp (Var "x");
//         Assign ("x", Sub (Var "x", Literal 1.0))
//     ]);
//     PrintStr "Done!"
// ]

// let pythonCode = program |> List.map (statementToPython "") |> String.concat "\n"
// printfn "%s" pythonCode
