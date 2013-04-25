(*

Copyright 2008-2012 Microsoft Corporation

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

(* NOTE : The tests here were adapted from the ArrayModule tests
          in the F# 3.0 code drop. *)

/// Unit tests for the ExtCore.Collections.Vector module.
module Tests.ExtCore.Collections.Vector

open System
open NUnit.Framework
open FsUnit

(*
[Test Strategy]
Make sure each method works on:
* Integer vector (value type)
* String  vector (reference type)
* Empty   vector (0 elements)
* Null    vector (null)
*)

let rec private IsNaN (x : obj) =
    match x with
    | :? float   as x -> Double.IsNaN(x)
    | :? float32 as x -> Single.IsNaN(x)
    | :? decimal as x -> Decimal.ToDouble(x) |> box |> IsNaN
    | _ -> failwith "Invalid input. Please provide a numeric type which could possibly be NaN"

[<TestCase>]
let empty () : unit =
    let emptyVector = Vector.empty
    if Vector.length emptyVector <> 0 then Assert.Fail()

[<TestCase>]
let append () : unit =
    // integer vector
    Vector.append
        (vector [| 1; 2 |])
        (vector [| 3; 4 |])
    |> should equal
        (vector [| 1; 2; 3; 4 |])
        
    // string vector
    Vector.append
        (vector [| "a"; "b" |])
        (vector [| "C"; "D" |])
    |> should equal
        (vector [| "a"; "b"; "C"; "D" |])

    // empty vector
    let emptyArray : vector<int>  = vector [|   |]
    let singleArray : vector<int> = vector [| 1 |]
        
    let appEmptySingle = Vector.append emptyArray singleArray
    let appSingleEmpty = Vector.append singleArray emptyArray
        
    Assert.IsTrue( (appEmptySingle = vector [| 1 |]) )
    Assert.IsTrue( (appSingleEmpty = vector [| 1 |]) )
      
    // null vector
    let nullArray : vector<int> = Unchecked.defaultof<vector<int>>
    let validArray = vector [| 1 |]
    checkThrowsArgumentNullException (fun () -> Vector.append validArray nullArray |> ignore)    
    checkThrowsArgumentNullException (fun () -> Vector.append nullArray validArray |> ignore)   

    ()

[<TestCase>]
let average () : unit =   
    // empty float32 vector
    let emptyFloatArray = Vector.empty<float32> 
    checkThrowsArgumentException(fun () -> Vector.average emptyFloatArray |> ignore)
        
    // empty double vector
    let emptyDoubleArray = Vector.empty<float> 
    checkThrowsArgumentException(fun () -> Vector.average emptyDoubleArray |> ignore)
        
    // empty decimal vector
    let emptyDecimalArray = Vector.empty<decimal> 
    checkThrowsArgumentException (fun () -> Vector.average emptyDecimalArray |>ignore )

    // float32 vector
    let floatArray: vector<float32> = vector [| 1.2f; 3.5f; 6.7f |]
    let averageOfFloat = Vector.average floatArray
    if averageOfFloat <> 3.8000000000000003f then Assert.Fail()
        
    // double vector
    let doubleArray: vector<float> = vector [| 1.0;8.0 |]
    let averageOfDouble = Vector.average doubleArray
    if averageOfDouble <> 4.5 then Assert.Fail()
        
    // decimal vector
    let decimalArray: vector<decimal> = vector [| 0M; 19M; 19.03M |]
    let averageOfDecimal = Vector.average decimalArray
    if averageOfDecimal <> 12.676666666666666666666666667M then Assert.Fail()      
        
    // null vector
    let nullArr : vector<double> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.average nullArr |> ignore) 

    ()
        
[<TestCase>]
let averageBy () : unit =  
    
    // empty double vector   
    let emptyDouArray = Vector.empty<float>
    let funcd x = x + 6.7
    checkThrowsArgumentException(fun () -> Vector.averageBy funcd emptyDouArray |> ignore)
                
    // empty float32 vector
    let emptyFloat32Array: vector<float32> = vector [||]
    let funcf x = x + 9.8f 
    checkThrowsArgumentException(fun () -> Vector.averageBy funcf emptyFloat32Array |> ignore)
        
    // empty decimal vector
    let emptyDecimalArray = Vector.empty<decimal>
    let funcDecimal x = x + 9.8M 
    checkThrowsArgumentException(fun () -> Vector.averageBy funcDecimal emptyDecimalArray |> ignore)
        
    // float32 vector
    let floatArray: vector<float32> = vector [| 1.2f;3.5f;6.7f |]      
    let averageOfFloat = Vector.averageBy funcf floatArray
    if averageOfFloat <> 13.5999994f then Assert.Fail()
        
    // double vector
    let doubleArray: vector<float> = vector [| 1.0;8.0 |]
    let averageOfDouble = Vector.averageBy funcd doubleArray
    if averageOfDouble <> 11.2 then Assert.Fail()
        
    // decimal vector
    let decimalArray: vector<decimal> = vector [| 0M;19M;19.03M |]
    let averageOfDecimal = Vector.averageBy funcDecimal decimalArray
    if averageOfDecimal <> 22.476666666666666666666666667M then Assert.Fail()     
        
    // null vector
    let nullArr : vector<double> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.averageBy funcd nullArr |> ignore) 
        
    ()
        
[<TestCase>]
let blit () : unit = 
    // int vector   
    let intSrc = vector [| 1..10 |]
    let intDes:vector<int> = Vector.zeroCreate 10 
    let intResult = Vector.blit intSrc 0 intDes 0 5
    if intResult.[4] <> 5 then Assert.Fail()
    if intResult.[5] <> 0 then Assert.Fail()
        
    // string vector
    let strSrc = vector [| "a";"b";"c";"d";"e";"j"|]
    let strDes = Vector.create 10 "w"
    let strResult = Vector.blit strSrc 1 strDes 2 3
    if strResult.[3] <> "c" || Vector.get strResult 4 = "w" then Assert.Fail()
     
    // null vector
    let nullArr :vector<string> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.blit nullArr 1 strDes 2 3 |> ignore) 

    // bounds check
    checkThrowsArgumentException (fun () -> Vector.blit intSrc -1 intDes 1 3 |> ignore)
    checkThrowsArgumentException (fun () -> Vector.blit intSrc 1 intDes -1 3 |> ignore)
    checkThrowsArgumentException (fun () -> Vector.blit intSrc 1 intDes 1 -3 |> ignore)
    checkThrowsArgumentException (fun () -> Vector.blit intSrc 1 intDes 1 300 |> ignore)
    checkThrowsArgumentException (fun () -> Vector.blit intSrc 1 intDes 5 8 |> ignore)
        
    ()

      
let private ChooseTester chooseInt chooseString = 
    // int vector
    let intSrc:vector<int> = vector [| 1..100 |]    
    let funcInt x = if (x%5=0) then Some x else None       
    let intChoosed : vector<int> = chooseInt funcInt intSrc
    if intChoosed.[1] <> 10 then Assert.Fail()
        
    // string vector
    let stringSrc: vector<string> =
        "Lists are a commonly used data structure. They are not mutable, i.e., you can't delete an element of a list – instead you create a new list with the element deleted. List values often share storage under the hood, i.e., a list value only allocate more memory when you actually execute construction operations."
            .Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |> vector

    let funcString x = match x with
                        | "list"-> Some x
                        | "List" -> Some x
                        | _ -> None
    let strChoosed : vector<string>  = chooseString funcString stringSrc   
    if strChoosed.[1].ToLower() <> "list" then Assert.Fail()
        
    // empty vector
    let emptySrc :vector<int> = vector [| |]
    let emptyChoosed = chooseInt funcInt emptySrc
    Assert.IsTrue( (emptyChoosed = vector [| |]) )

    // null vector
    let nullArr :vector<int> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> chooseInt funcInt nullArr |> ignore) 
        
    () 
      
[<TestCase>]
let choose () : unit = 
    ChooseTester Vector.choose Vector.choose

let private CollectTester collectInt collectString =
    
    // int vector - checking ordering
    let intSrc = vector [| 1..3 |]
    let func = fun i -> vector [| 1..i |]
    let result : vector<int> = collectInt func intSrc
    Assert.AreEqual (vector [| 1; 1; 2; 1; 2; 3 |], result)
        
    // string vector
    let stringSrc = vector [| "foo"; "bar" |]
    let func = fun s -> vector [| s |]
    let result : vector<string> = collectString func stringSrc
    Assert.AreEqual(stringSrc, result)
        
    // empty vector
    let emptyArray : vector<string> = vector [| |]
    let result = collectString func emptyArray
    Assert.AreEqual(emptyArray, result)
        
    // null vector
    let nullArr :vector<int> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> collectInt func nullArr |> ignore)
        
    ()

[<TestCase>]
let collect  () : unit =
    CollectTester Vector.collect Vector.collect
        
[<TestCase>]
let collectWithSideEffects  () : unit =
    let stamp = ref 0
    let f x = stamp := !stamp + 1; vector [| x |]
        
    Vector.collect f (vector [| |]) |> ignore
    Assert.AreEqual(0, !stamp)
        
    stamp := 0
    Vector.collect f (vector [|1;2;3|]) |> ignore
    Assert.AreEqual(3,!stamp)
        
[<TestCase>]
let concat () : unit =
    // integer vector
    let seqInt = 
        seq { for i in 1..10 do                
                yield vector [|i; i*10|] }
                    
    let conIntArr = Vector.concat seqInt
    if Vector.length conIntArr <> 20 then Assert.Fail()
        
    // string vector
    let strSeq = 
        seq { for a in 'a'..'c' do
                for b in 'a'..'c' do
                    yield vector [|a.ToString();b.ToString() |]}
     
    let conStrArr = Vector.concat strSeq
    if Vector.length conStrArr <> 18 then Assert.Fail()
        
    // Empty vector
    let emptyArrays = [| vector [| |]; vector [| 0 |]; vector [| 1 |]; vector [| |]; vector [| |] |]
    let result2 = Vector.concat emptyArrays
    Assert.IsTrue(result2.[0] = 0 && result2.[1] = 1)
    if result2.[0] <> 0 && result2.[1] <> 1 then Assert.Fail()    

    // null vector
    let nullArray :vector<int> = Unchecked.defaultof<vector<_>>
    let nullArrays = Vector.create 2 nullArray
    checkThrowsNullRefException (fun () -> Vector.concat nullArrays |> ignore) 
                
    () 
        

[<TestCase>]
let copy () : unit =
    // int vector
    let intSrc:vector<int> = vector [| 3;5;7 |]    
    let intCopyed = Vector.copy  intSrc
    if intCopyed <> vector [| 3;5;7 |] then Assert.Fail()
        
    // string vector
    let stringSrc: vector<string> = vector [|"Lists"; "are";  "commonly"  |]
        
    let strCopyed = Vector.copy  stringSrc   
    if strCopyed <> vector [|"Lists"; "are";  "commonly"  |] then Assert.Fail()
        
    // empty vector
    let emptySrc :vector<int> = vector [| |]
    let emptyCopyed = Vector.copy emptySrc
    if emptyCopyed <> vector [| |] then Assert.Fail()

    // null vector
    let nullArr :vector<int> = Unchecked.defaultof<vector<_>>    
    checkThrowsArgumentNullException (fun () -> Vector.copy nullArr |> ignore) 
        
    ()

[<TestCase>]
let create () : unit =
    // int vector
    let intArr = Vector.create 3 8    
    if intArr <> vector [| 8;8;8 |] then Assert.Fail()
        
    // string vector
    let strArr = Vector.create 3 "good"
    Assert.IsTrue( (strArr = vector [|"good"; "good";  "good"|]) )
        
    // empty vector
    let emptyArr = Vector.create 0 "empty"    
    if emptyArr <> vector [| |] then Assert.Fail()

    // vector with null elements
    let nullStr : string = null
    let nullArr = Vector.create 3 nullStr
    Assert.IsTrue( (nullArr = vector [|null; null; null|]) )
        
    ()
        
[<TestCase>]
let exists () : unit =
    // integer vector
    let intArr = vector [| 2;4;6;8 |]
    let funcInt x = if (x%2 = 0) then true else false
    let resultInt = Vector.exists funcInt intArr
    if resultInt <> true then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" |]
    let funcStr (x:string) = if (x.Length >15) then true else false
    let resultStr = Vector.exists funcStr strArr
    if resultStr <> false then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = Vector.exists funcInt emptyArr
    if resultEpt <> false then Assert.Fail()

    // null vector
    let nullArr :vector<string>= Unchecked.defaultof<vector<_>>      
    checkThrowsArgumentNullException (fun () -> Vector.exists funcStr nullArr |> ignore) 
        
    ()
        
[<TestCase>]
let exists2 () : unit =
    // integer vector
    let intFir = vector [| 2;4;6;8 |]
    let intSec = vector [| 1;2;3;4 |]
    let funcInt x y = if (x%y = 0) then true else false
    let resultInt = Vector.exists2 funcInt intFir intSec
    if resultInt <> true then Assert.Fail()
        
    // string vector
    let strFir = vector [|"Lists"; "are";  "commonly" |]
    let strSec = vector [|"good"; "good";  "good"  |]
    let funcStr (x:string) (y:string) = if (x = y) then true else false
    let resultStr = Vector.exists2 funcStr strFir strSec
    if resultStr <> false then Assert.Fail()
        
    // empty vector
    let eptFir:vector<int> = vector [| |]
    let eptSec:vector<int> = vector [| |]
    let resultEpt = Vector.exists2 funcInt eptFir eptSec
    if resultEpt <> false then Assert.Fail()

    // null vector
    let nullFir :vector<string> = Unchecked.defaultof<vector<_>>
    let validArray = vector [| "a" |]      
    checkThrowsArgumentNullException (fun () -> Vector.exists2 funcStr nullFir validArray |> ignore)  
    checkThrowsArgumentNullException (fun () -> Vector.exists2 funcStr validArray nullFir |> ignore) 
        
    // len1 <> len2
    checkThrowsArgumentException(fun () -> Vector.exists2 funcInt (vector [|1..10|]) (vector [|2..20|]) |> ignore)
        
    ()

[<TestCase>]
let fill () : unit =
    // integer vector
    let intArr = vector [|1..5|]
    let intResult = Vector.fill intArr 0 3 21
    if intResult <> vector [|21;21;21;4;5|] then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
    let strResult = Vector.fill strArr 1 5 "a"
        
    if strResult <> vector [|"Lists"; "a"; "a"; "a"; "a";"a" |] then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    let emptyResult = Vector.fill emptyArr 0 0 8
    if emptyResult <> vector [| |] then Assert.Fail()

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.fill nullArr 0 1 "good" |> ignore)
        
    // start < 0
    checkThrowsArgumentException(fun () -> Vector.fill intArr -1 3 21 |> ignore)
        
    // len < 0        
    checkThrowsArgumentException(fun () -> Vector.fill intArr 1 -2 21 |> ignore)
        
         
    ()

[<TestCase>] 
let filter () : unit =
    // integer vector
    let intArr = vector [| 1..20 |]
    let funcInt x = if (x%5 = 0) then true else false
    let resultInt = Vector.filter funcInt intArr
    if resultInt <> vector [|5;10;15;20|] then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
    let funcStr (x:string) = if (x.Length > 4) then true else false
    let resultStr = Vector.filter funcStr strArr
    if resultStr <> vector [|"Lists";  "commonly"; "structor" |] then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = Vector.filter funcInt emptyArr
    if resultEpt <> vector [| |] then Assert.Fail()

    // null vector
    let nullArr :vector<string> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () ->  Vector.filter funcStr nullArr |> ignore) 
        
    ()   

[<TestCase>]
let find () : unit =
    // integer vector
    let intArr = vector [| 1..20 |]
    let funcInt x = if (x%5 = 0) then true else false
    let resultInt = Vector.find funcInt intArr
    if resultInt <> 5 then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
    let funcStr (x:string) = if (x.Length >7) then true else false
    let resultStr = Vector.find funcStr strArr
    if resultStr <> "commonly" then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |] 
    checkThrowsKeyNotFoundException (fun () -> Vector.find (fun x -> true) emptyArr |> ignore)        

    // null vector
    let nullArr :vector<string> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.find funcStr nullArr |> ignore) 
        
    () 

[<TestCase>]
let findIndex () : unit =
    // integer vector
    let intArr = vector [| 1..20 |]
    let funcInt x = if (x%5 = 0) then true else false
    let resultInt = Vector.findIndex funcInt intArr
    if resultInt <> 4 then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are"; "a"; "commonly"; "data";"structor" |]
    let funcStr (x:string) = if (x.Length >7) then true else false
    let resultStr = Vector.findIndex funcStr strArr
    if resultStr <> 3 then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]  
    checkThrowsKeyNotFoundException(fun() -> Vector.findIndex (fun x -> true) emptyArr |> ignore) 
        

    // null vector
    let nullArr :vector<string> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.findIndex funcStr nullArr |> ignore) 
        
    () 
        
[<TestCase>]
let pick () : unit =
    // integers
    let intArr = vector [| 1..10 |]
    let matchFunc n =
        if n = 3 then Some(n.ToString())
        else None
    let resultInt = Vector.pick matchFunc intArr
    Assert.AreEqual("3", resultInt)
        
    // make it not found
    checkThrowsKeyNotFoundException (fun () -> Vector.pick (fun n -> None) intArr |> ignore)
        
[<TestCase>]
let toSeq () : unit =
    let intArr = vector [| 1..10 |]
    let seq = Vector.toSeq intArr
    let sum = Seq.sum seq
    Assert.AreEqual(55, sum)
        
[<TestCase>]
let tryPick () : unit =
    // integer vector
    let intArr = vector [| 1..10 |]    
    let funcInt x = 
            match x with
            | _ when x % 3 = 0 -> Some (x.ToString())            
            | _ -> None
    let resultInt = Vector.tryPick funcInt intArr
    if resultInt <> Some "3" then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" ; "list" |]
    let funcStr x = 
            match x with
            | "good" -> Some (x.ToString())            
            | _ -> None
    let resultStr = Vector.tryPick funcStr strArr
    if resultStr <> None then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = Vector.tryPick funcInt emptyArr
    if resultEpt <> None then Assert.Fail()

    // null vector
    let nullArr :vector<string>= Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.tryPick funcStr nullArr |> ignore)  
        
    ()

[<TestCase>]
let fold () : unit =
    // integer vector
    let intArr = vector [| 1..5 |]    
    let funcInt x y = x+"+"+y.ToString()
    let resultInt = Vector.fold funcInt "x" intArr
    if resultInt <> "x+1+2+3+4+5" then Assert.Fail()
        
    // string vector
    let strArr = vector [|"A"; "B";  "C" ; "D" |]
    let funcStr x y = x+y
            
    let resultStr = Vector.fold funcStr "X" strArr
    if resultStr <> "XABCD" then Assert.Fail()
        
    // empty vector
    let emptyArr : vector<int> = vector [| |]
    let resultEpt = Vector.fold funcInt "x" emptyArr
    if resultEpt <> "x" then Assert.Fail()

    // null vector
    let nullArr :vector<string>= Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> Vector.fold funcStr "begin" nullArr |> ignore)  
        
    ()

[<TestCase>]
let fold2 () : unit =
    // integer vector  
    let funcInt x y z = x + y.ToString() + z.ToString()
    let resultInt = Vector.fold2 funcInt "x" (vector [| 1;3;5 |]) (vector [|2;4;6|])
    if resultInt <> "x123456" then Assert.Fail()
        
    // string vector
    let funcStr x y z= x + y + z        
    let resultStr = Vector.fold2 funcStr "X" (vector [|"A"; "B";  "C" ; "D" |]) (vector [|"H"; "I";  "J" ; "K" |])
    if resultStr <> "XAHBICJDK" then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = Vector.fold2 funcInt "x" emptyArr emptyArr
    if resultEpt <> "x" then Assert.Fail()

    // null vector
    let nullArr :vector<string> = Unchecked.defaultof<vector<_>>
    let validArray = vector [| "a" |]
    checkThrowsArgumentNullException (fun () -> Vector.fold2 funcStr "begin" validArray nullArr |> ignore)  
    checkThrowsArgumentNullException (fun () -> Vector.fold2 funcStr "begin" nullArr validArray |> ignore)  
        
    // len1 <> len2
    checkThrowsArgumentException(fun () -> Vector.fold2 funcInt "x" (vector [| 1;3;5 |]) (vector [|2;4;6;8|]) |> ignore)
                
    ()

[<TestCase>]
let foldBack () : unit =
    // integer vector
    let intArr = vector [| 1..5 |]    
    let funcInt x y = x.ToString()+y
    let resultInt = Vector.foldBack funcInt intArr "x"
    if resultInt <> "12345x" then Assert.Fail()
        
    // string vector
    let strArr = vector [|"A"; "B";  "C" ; "D" |]
    let funcStr x y = x+y
            
    let resultStr = Vector.foldBack funcStr strArr "X" 
    if resultStr <> "ABCDX" then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = Vector.foldBack funcInt emptyArr "x" 
    if resultEpt <> "x" then Assert.Fail()

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>>      
    checkThrowsArgumentNullException (fun () -> Vector.foldBack funcStr nullArr "begin" |> ignore)  
        
    ()

[<TestCase>]
let foldBack2 () : unit =
    // integer vector  
    let funcInt x y z = x.ToString() + y.ToString() + z
    let resultInt = Vector.foldBack2 funcInt (vector [| 1;3;5 |]) (vector [|2;4;6|]) "x"
    if resultInt <> "123456x" then Assert.Fail()
        
    // string vector
    let funcStr x y z= x + y + z        
    let resultStr = Vector.foldBack2 funcStr (vector [|"A"; "B";  "C" ; "D" |]) (vector [|"H"; "I";  "J" ; "K" |]) "X"
    if resultStr <> "AHBICJDKX" then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = Vector.foldBack2 funcInt emptyArr emptyArr "x"
    if resultEpt <> "x" then Assert.Fail()

    // null vector
    let nullArr = Unchecked.defaultof<vector<_>> : vector<string> 
    let validArray = vector [| "a" |] 
    checkThrowsArgumentNullException (fun () -> Vector.foldBack2 funcStr nullArr validArray "begin" |> ignore)  
    checkThrowsArgumentNullException (fun () -> Vector.foldBack2 funcStr validArray nullArr "begin" |> ignore)  
        
    // len1 <> len2
    checkThrowsArgumentException(fun () -> Vector.foldBack2 funcInt (vector [|1..10|]) (vector [|2..20|]) "x" |> ignore)
        
    ()

[<TestCase>]
let forall () : unit =
    // integer vector
    let resultInt = Vector.forall (fun x -> x > 2) <| vector [| 3..2..10 |]
    if resultInt <> true then Assert.Fail()
        
    // string vector
    let resultStr = Vector.forall (fun (x:string) -> x.Contains("a")) <| vector [|"Lists"; "are";  "commonly" ; "list" |]
    if resultStr <> false then Assert.Fail()
        
    // empty vector 
    let resultEpt = Vector.forall (fun (x:string) -> x.Contains("a")) <| vector [||] 
    if resultEpt <> true then Assert.Fail()

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>> 
    checkThrowsArgumentNullException (fun () -> Vector.forall (fun x -> true) nullArr |> ignore)  
        
    ()
        
[<TestCase>]
let forall2 () : unit =
    // integer vector
    let resultInt = Array.forall2 (fun x y -> x < y) [| 1..10 |] [|2..2..20|]
    if resultInt <> true then Assert.Fail()
        
    // string vector
    let resultStr =
        Vector.forall2 (fun (x:string) (y:string) -> x.Length < y.Length)
            (vector [|"Lists"; "are";  "commonly" ; "list" |])
            (vector [|"Listslong"; "arelong";  "commonlylong" ; "listlong" |])
    if resultStr <> true then Assert.Fail()
        
    // empty vector 
    let resultEpt = Vector.forall2 (fun x y -> x>y) (vector [||]) (vector [||])
    if resultEpt <> true then Assert.Fail()

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>>
    let validArray = vector [| "a" |] 
    checkThrowsArgumentNullException (fun () -> Vector.forall2 (fun x y-> true) nullArr validArray |> ignore)  
    checkThrowsArgumentNullException (fun () -> Vector.forall2 (fun x y-> true) validArray nullArr |> ignore)  
        
    // len1 <> len2
    checkThrowsArgumentException(fun () -> Vector.forall2 (fun x y -> x < y) (vector [|1..10|]) (vector [|2..20|]) |> ignore)
        
    ()
        
[<TestCase>]
let get () : unit =
    // integer vector
    let intArr = vector [| 3;4;7;8;10 |]    
    let resultInt = Vector.get intArr 3
    if resultInt <> 8 then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" ; "list" |]
        
    let resultStr = Vector.get strArr 2
    if resultStr <> "commonly" then Assert.Fail()
        
    // empty vector
    let emptyArr:vector<int> = vector [| |]
    checkThrowsIndexOutRangException (fun () -> Vector.get emptyArr -1 |> ignore)

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>> 
    checkThrowsNullRefException (fun () -> Vector.get nullArr 0 |> ignore)  
        
    ()

let private InitTester initInt initString = 
    // integer vector
    let resultInt : vector<int> = initInt 3 (fun x -> x + 3) 
    if resultInt <> vector [|3;4;5|] then Assert.Fail()
        
    // string vector
    let funStr (x:int) = 
        match x with
        | 0 -> "Lists"
        | 1 -> "are"
        | 2 -> "commonly"
        | _ -> "end"    
    let resultStr = initString 3 funStr
    if resultStr <> vector [|"Lists"; "are";  "commonly"  |] then Assert.Fail()
        
    // empty vector  
    let resultEpt = initInt 0 (fun x -> x+1)
    if resultEpt <> vector [| |] then Assert.Fail()
        
    ()

[<TestCase>]
let init () : unit = 
    InitTester Vector.init Vector.init
        
[<TestCase>]
let initWithSideEffects () : unit =
    let stamp = ref 0
    let f i = 
        stamp := !stamp + 1; 
        i 
    Vector.init 0 f |> ignore
    Assert.AreEqual (0, !stamp)
        
    stamp := 0
    Vector.init 10 f |> ignore
    Assert.AreEqual (10, !stamp)

[<TestCase>]
let isEmpty () : unit =
    // integer vector
    let intArr = vector [| 3;4;7;8;10 |]    
    let resultInt = Vector.isEmpty intArr 
    if resultInt <> false then Assert.Fail()
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" ; "list" |]    
    let resultStr = Vector.isEmpty strArr 
    if resultStr <> false then Assert.Fail()
        
    // empty vector    
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = Vector.isEmpty emptyArr 
    if resultEpt <> true then Assert.Fail()

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>> 
    checkThrowsArgumentNullException (fun () -> Vector.isEmpty nullArr |> ignore)  
        
    ()

[<TestCase>]
let iter () : unit =
    // integer vector
    let intArr = vector [| 1..10 |]  
    let resultInt = ref 0    
    let funInt (x:int) =   
        resultInt := !resultInt + x              
        () 
    Vector.iter funInt intArr 
    if !resultInt <> 55 then Assert.Fail()    
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" ; "list" |]
    let resultStr = ref ""
    let funStr (x : string) =
        resultStr := (!resultStr) + x   
        ()
    Vector.iter funStr strArr  
    if !resultStr <> "Listsarecommonlylist" then Assert.Fail()   
        
    // empty vector    
    let emptyArr : vector<int> = vector [| |]
    let resultEpt = ref 0
    Vector.iter funInt emptyArr 
    if !resultEpt <> 0 then Assert.Fail()    

    // null vector
    let nullArr = Unchecked.defaultof<vector<_>> : vector<string>  
    checkThrowsArgumentNullException (fun () -> Vector.iter funStr nullArr |> ignore)  
        
    ()
       
[<TestCase>]
let iter2 () : unit =
    // integer vector
    let resultInt = ref 0    
    let funInt (x:int) (y:int) =   
        resultInt := !resultInt + x + y             
        () 
    Vector.iter2 funInt (vector [| 1..10 |]) (vector [|2..2..20|])
    if !resultInt <> 165 then Assert.Fail()    
        
    // string vector
    let resultStr = ref ""
    let funStr (x:string) (y:string) =
        resultStr := (!resultStr) + x  + y 
        ()
    Vector.iter2 funStr (vector [|"A"; "B";  "C" ; "D" |]) (vector [|"a"; "b"; "c"; "d"|])
    if !resultStr <> "AaBbCcDd" then Assert.Fail()   
        
    // empty vector    
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = ref 0
    Vector.iter2 funInt emptyArr emptyArr 
    if !resultEpt <> 0 then Assert.Fail()    

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>>  
    let validArray = vector [| "a" |]     
    checkThrowsArgumentNullException (fun () -> Vector.iter2 funStr nullArr validArray |> ignore)  
    checkThrowsArgumentNullException (fun () -> Vector.iter2 funStr validArray nullArr |> ignore)  
        
    // len1 <> len2        
    checkThrowsArgumentException(fun () -> Vector.iter2 funInt (vector [| 1..10 |]) (vector [|2..20|]))
  
    ()
        
        
[<TestCase>]
let iteri () : unit =
    // integer vector
    let intArr = vector [| 1..10 |]  
    let resultInt = ref 0    
    let funInt (x:int) y =   
        resultInt := !resultInt + x + y             
        () 
    Vector.iteri funInt intArr 
    if !resultInt <> 100 then Assert.Fail()    
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" ; "list" |]
    let resultStr = ref 0
    let funStr (x:int) (y:string) =
        resultStr := (!resultStr) + x + y.Length
        ()
    Vector.iteri funStr strArr  
    if !resultStr <> 26 then Assert.Fail()   
        
    // empty vector    
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = ref 0
    Vector.iteri funInt emptyArr 
    if !resultEpt <> 0 then Assert.Fail()    

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>> 
    checkThrowsArgumentNullException (fun () -> Vector.iteri funStr nullArr |> ignore)  
        
    ()
        
[<TestCase>]
let iteri2 () : unit =
    // integer vector
    let resultInt = ref 0    
    let funInt (x:int) (y:int) (z:int) =   
        resultInt := !resultInt + x + y + z            
        () 
    Vector.iteri2 funInt (vector [| 1..10 |]) (vector [|2..2..20|])
    if !resultInt <> 210 then Assert.Fail()    
        
    // string vector
    let resultStr = ref ""
    let funStr (x:int) (y:string) (z:string) =
        resultStr := (!resultStr) + x.ToString()  + y + z
        ()
    Vector.iteri2 funStr (vector [|"A"; "B";  "C" ; "D" |]) (vector [|"a"; "b"; "c"; "d"|])
    if !resultStr <> "0Aa1Bb2Cc3Dd" then Assert.Fail()   
        
    // empty vector    
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = ref 0
    Vector.iteri2 funInt emptyArr emptyArr 
    if !resultEpt <> 0 then Assert.Fail()    

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>>
    let validArray = vector [| "a" |] 
    checkThrowsArgumentNullException (fun () -> Vector.iteri2 funStr nullArr validArray |> ignore)  
    checkThrowsArgumentNullException (fun () -> Vector.iteri2 funStr validArray nullArr |> ignore)  
        
    // len1 <> len2
    checkThrowsArgumentException(fun () -> Vector.iteri2 funInt (vector [| 1..10 |]) (vector [|2..20|])  |> ignore)
        
    ()                

let private MapTester mapInt (mapString : (string -> int) -> vector<string> -> vector<int>) =
    // empty vector 
    let f x = x + 1
    let result = mapInt f <| vector [| |]
    if result <> vector [| |] then Assert.Fail ()
        
    // int vector
    let result = mapInt f <| vector [| 1..100 |]
    if result <> vector [| 2..101 |] then Assert.Fail ()
        
    // string vector
    let result = vector [| "a"; "aa"; "aaa" |] |> mapString String.length
    if result <> vector [| 1..3 |] then Assert.Fail ()
        
    // null vector
    let nullArg : vector<int> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> mapInt f nullArg |> ignore)
        
    ()
        
[<TestCase>]  
let map () : unit =
    MapTester Vector.map Vector.map
        
[<TestCase>]
let mapWithSideEffects  () : unit =
    let stamp = ref 0
    let f x = stamp := !stamp + 1; x + 1
        
    Array.map f [| |] |> ignore
    Assert.AreEqual(0,!stamp)
        
    stamp := 0
    Array.map f [| 1..100 |] |> ignore
    Assert.AreEqual(100,!stamp)

let private MapiTester mapiInt mapiString =
    // empty vector 
    let f i x = (i, x + 1)
    let result = mapiInt f Vector.empty
    if result <> Vector.empty then Assert.Fail ()
        
    // int vector
    let result : vector<int*int> = mapiInt f <| Vector.ofArray [| 1..2 |]
    if result <> Vector.ofArray [| (0,2); (1,3) |] then Assert.Fail ()
        
    // string vector
    let result : vector<int*int> = Vector.ofArray [| "a"; "aa"; "aaa" |] |> mapiString (fun i (s:string) -> i, s.Length) 
    if result <> Vector.ofArray [| (0,1); (1,2); (2,3) |] then Assert.Fail ()
        
    // null vector
    let nullArg : vector<int> = Unchecked.defaultof<vector<_>>
    checkThrowsArgumentNullException (fun () -> mapiInt f nullArg |> ignore)        
    ()

[<TestCase>]
let mapi () : unit = MapiTester Vector.mapi Vector.mapi

[<TestCase>]
let mapiWithSideEffects  () : unit =
    let stamp = ref 0
    let f i x = stamp := !stamp + 1; (i, x + 1)
       
    Array.mapi f [| |] |> ignore
    Assert.AreEqual(0,!stamp)
       
    stamp := 0
    Array.mapi f [| 1..100 |] |> ignore
    Assert.AreEqual(100,!stamp)
    ()
            
let private PartitionTester partInt partString =
    // int vector
    let intSrc:vector<int> = vector [| 1..100 |]    
    let funcInt x = if (x%2=1) then true else false
    let intPartitioned : vector<int> * vector<int> = partInt funcInt intSrc
    if (vector [|1..2..100|], vector [|2..2..100|]) <> intPartitioned then Assert.Fail ()
        
    let allLeft = partInt (fun _ -> true) intSrc
    if (intSrc, vector [||]) <> allLeft then Assert.Fail()
    let allRight = partInt (fun _ -> false) intSrc
    if (vector [||], intSrc) <> allRight then Assert.Fail()

        
    // string vector
    let stringSrc: vector<string> =
        vector <| "List 1 list 2 3 4 5".Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    let funcString x = match x with
                        | "list"-> true
                        | "List" -> true
                        | _ -> false
    let strPartitioned : vector<string> * vector<string>  = partString funcString stringSrc   
    if strPartitioned <> (vector [|"List";"list"|], vector [| "1";"2"; "3"; "4"; "5"|]) then Assert.Fail ()
        
    // empty vector
    let emptySrc :vector<int> = vector [| |]
    let emptyPartitioned = partInt funcInt emptySrc
    if emptyPartitioned <> (vector [| |], vector [| |]) then Assert.Fail()
        
    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>> 
    checkThrowsArgumentNullException (fun () -> partString funcString nullArr |> ignore)


[<TestCase>]
let partition () : unit =
    PartitionTester Vector.partition Vector.partition    


(* Tests for the Array.Parallel module. *)
#if FX_NO_TPL_PARALLEL
#else
[<TestCase>]
let ``Parallel.choose``  () : unit = 
    ChooseTester Vector.Parallel.choose Vector.Parallel.choose

[<TestCase>]
let ``Parallel.collect``  () : unit =
    CollectTester Vector.Parallel.collect Vector.Parallel.collect

[<TestCase>]
let ``Parallel.init`` () : unit = 
    InitTester Vector.Parallel.init Vector.Parallel.init

[<TestCase>]
let ``Parallel.map``  () : unit =
    MapTester Vector.Parallel.map Vector.Parallel.map

[<TestCase>]
let ``Parallel.mapi``  () : unit =
    MapiTester Vector.Parallel.mapi Vector.Parallel.mapi
    ()
        
[<TestCase>]
let ``Parallel.iter`` () : unit =
    // integer vector
    let intArr = vector [| 1..10 |]  
    let resultInt = ref 0    
    let funInt (x:int) =   
        lock resultInt (fun () -> resultInt := !resultInt + x)
        () 
    Vector.Parallel.iter funInt intArr 
    if !resultInt <> 55 then Assert.Fail()    
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" ; "list" |]
    let resultStr = ref 0
    let funStr (x : string) =
        lock resultStr (fun () -> resultStr := (!resultStr) + x.Length)
        ()
    Vector.Parallel.iter funStr strArr  
    if !resultStr <> 20 then Assert.Fail()   
        
    // empty vector    
    let emptyArr : vector<int> = vector [| |]
    let resultEpt = ref 0
    Vector.Parallel.iter funInt emptyArr 
    if !resultEpt <> 0 then Assert.Fail()    

    // null vector
    let nullArr = Unchecked.defaultof<vector<_>> : vector<string>  
    checkThrowsArgumentNullException (fun () -> Vector.Parallel.iter funStr nullArr |> ignore)  
        
    ()
        
[<TestCase>]
let ``Parallel.iteri`` () : unit =   
    // integer vector
    let intArr = vector [| 1..10 |] 
                 
    let resultInt = ref 0    
    let funInt (x:int) y =   
        lock resultInt (fun () -> resultInt := !resultInt + x + y)
        () 
    Vector.Parallel.iteri funInt intArr 
    if !resultInt <> 100 then Assert.Fail()    
        
    // string vector
    let strArr = vector [|"Lists"; "are";  "commonly" ; "list" |]
    let resultStr = ref 0
    let funStr (x:int) (y:string) =
        lock resultStr (fun () -> resultStr := (!resultStr) + x + y.Length)
        ()
    Vector.Parallel.iteri funStr strArr  
    if !resultStr <> 26 then Assert.Fail()   
        
    // empty vector    
    let emptyArr:vector<int> = vector [| |]
    let resultEpt = ref 0
    Vector.Parallel.iteri funInt emptyArr 
    if !resultEpt <> 0 then Assert.Fail()    

    // null vector
    let nullArr : vector<string> = Unchecked.defaultof<vector<_>> 
    checkThrowsArgumentNullException (fun () -> Vector.Parallel.iteri funStr nullArr |> ignore)  
        
    ()

[<TestCase>]
let ``Parallel.partition``  () : unit =
    PartitionTester Vector.Parallel.partition Vector.Parallel.partition    
#endif    



