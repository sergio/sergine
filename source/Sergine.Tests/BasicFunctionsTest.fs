namespace SergineTests

open BasicFunctions

module BasicFunctionsTest =

    let ``sum returns correct result`` () =
        let result = sum 1 2
        printfn "%A" result