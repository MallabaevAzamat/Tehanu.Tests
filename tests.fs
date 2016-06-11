namespace Tehanu

open NUnitLite

open NUnit.Framework

open Tehanu.Core
open Tehanu.Core.Generators
open Tehanu.FSharp
open Tehanu.FSharp.Quote
open Tehanu.FStar.Printer

[<TestFixture>]
module Tests =
  [<Test>]
  let ``Atom "laplandia" returns Atom "laplandia"`` () =
    let result = string <| Atom "laplandia"
    printfn "result = %s" result
    Assert.AreEqual(result, string <| Atom "laplandia")                                  

  [<Test>]
  let ``Pair(ref <| Atom "lalala", ref <| Atom "dadada") returns Pair(ref <| Atom "lalala", ref <| Atom "dadada")`` () =
    let result = string <| Pair(ref <| Atom "lalala", ref <| Atom "dadada")
    printfn "result = %s" result
    Assert.AreEqual(result, string <| Pair(ref <| Atom "lalala", ref <| Atom "dadada"))
  
  [<Test>]
  let ``genList [] returns Atom "nil"`` () =
    let result =  string <| genList []
    printfn "result = %s" result
    Assert.AreEqual(result, string <| Atom "nil")

  [<Test>]
  let ``genList [Atom "haha"; Atom "lala"] returns Pair(ref <| Atom "haha", ref <| Pair(ref <| Atom "lala", ref <| Atom "nil"))`` () =
    let result = string <| genList [Atom "haha"; Atom "lala"]
    printfn "result = %s" result
    Assert.AreEqual(result, string <| Pair(ref <| Atom "haha", ref <| Pair(ref <| Atom "lala", ref <| Atom "nil")))

  [<Test>]
  let ``genId "hahatunchik" returns Atom <| "`" + "`hahatunchik`" + "`"`` () =
    let result = string <| genId "hahatunchik"
    printfn "result = %s" result
    Assert.AreEqual(result, string (Atom <| "`" + "`hahatunchik`" + "`"))

  [<Test>]
  let ``genInt 42 returns Atom "42"`` () =
    let result = string <| genInt 42
    printfn "result = %s" result
    Assert.AreEqual(result, string <| Atom "42")

  [<Test>]                                                                                                                 
  let ``sprintModules <| toTree "/home/user/oh.fs" "module Oh\n[<Total(\"forall x y . x < y ==> i x + 2 <= i y\")>]\nlet f (x: int): int = if x > 1 then x * x * x else x - 2"`` () =
    let result = sprintModules <| toTree "/home/user/oh.fs" "module Oh\n[<Total(\"forall x y . x < y ==> i x + 2 <= i y\")>]\nlet f (x: int): int = if x > 1 then x * x * x else x - 2"
    printfn "result = %s" (string result)

  [<Test>]
  let ``sprintModules <| toTree "/home/user/oh.fs" "module Oh\n[<Total(\"forall x y . x < y ==> i x < i y\")>]\nlet g (x: int): int = x * x"`` () =
    let result = sprintModules <| toTree "/home/user/oh.fs" "module Oh\n[<Total(\"forall x y . x < y ==> i x < i y\")>]\nlet g (x: int): int = x * x"
    printfn "result = %s" (string result)

  [<Test>]
  let ``sprintModules <| toTree "/home/user/oh.fs" "module Oh\n[<Total \"forall x y . x > y <==> i x > i y\">] let f (x: int, y: int) (z: int): xx:int = x * x * x"`` () =
    let result = sprintModules <| toTree "/home/user/oh.fs" "module Oh\n[<Total \"forall x y . x > y <==> i x > i y\">] let f (x: int, y: int) (z: int): xx:int = x * x * x"
    printfn "result = %s" (string result)

  [<Test>]
  let ``createModule "Oh" ["haha", "forall x . x > 0 <==> i x > 0", <@@fun x -> x * x@@>] |> sprintModule |> printfn "result = %s"`` () = 
    createModule "Oh" ["haha",
                       "forall x . x > 0 <==> i x > 0",
                       <@@fun x -> x * x@@>]
    |> sprintModule
    |> string
    |> printfn "result = %s"           

  [<EntryPoint>]
  let main args =
    (AutoRun ()).Execute args