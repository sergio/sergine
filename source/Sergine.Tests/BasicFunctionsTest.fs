namespace SergineTests

open BasicFunctions
open NUnit.Framework
open Swensen.Unquote

module BasicFunctionsTest =

    [<Test>]
    let ``sum returns correct result`` () =
        let result = sum 1 2
        test <@ result = 3 @>