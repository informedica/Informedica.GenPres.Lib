module Tests

open Expecto

open MathNet.Numerics

module VU = Informedica.GenUnits.Lib.ValueUnit

let toString = VU.toString VU.Units.English VU.Units.Short

let (>>*) u f = 
    u |> toString |> printfn "%s"
    f u

// Some basic units
let mg400 = 400N |> VU.create VU.Units.Mass.milliGram
let gram2 = 2N |> VU.create VU.Units.Mass.gram
let ml50  = 50N  |> VU.create VU.Units.Volume.milliLiter
let ml5  = 5N    |> VU.create VU.Units.Volume.milliLiter
let l5 = 5N      |> VU.create VU.Units.Volume.liter 
let day2 = 2N |> VU.create VU.Units.Time.day
let hour3 = 3N |> VU.create VU.Units.Time.hour

// The count group is a special unit group 
// with only one unit: times. 
let times3 = 3N |> VU.create VU.Units.Count.times
let times100 = 100N |> VU.create VU.Units.Count.times


let equals exp res = Expect.equal res exp ""


[<Tests>]
let unitTests =


    testList "Unit" [
        test "base value of 400 mg = 0.4 g" {
            Expect.isTrue (mg400 |> VU.toBase = (400N/1000N)) ""
        }

        test "base value of 50 ml = 0.05 l" {
            Expect.isTrue (ml50 |> VU.toBase = (50N/1000N)) ""
        }

        test "base value of 5 ml = 0.005 l" {
            Expect.isTrue (ml5 |> VU.toBase = (5N/1000N)) ""
        }

        test "base value of 5 l = 5 l" {
            Expect.isTrue (l5 |> VU.toBase = (5N/1N)) ""
        }        
        
        test "count 3 times 5 ml results in 15 ml" {
            Expect.isTrue (ml5 * times3 |> VU.toBase = (3N * 5N) / 1000N) " "
        }
    ]


[<Tests>]
let comparisonTests =

    testList "Comparison" [

        test "ml50 < l5 using normal operator < should be false" {
            Expect.isFalse (ml50 < l5) ""
        }

        test "ml50 < l5 using special operator <?" {
            Expect.isTrue (ml50 <? l5) ""
        }

        test "ml50 = l5 should be false" {
            Expect.isFalse (ml50 =? l5) ""
        }

        test "ml50 * times 100 = l5 should be true" {
            Expect.isTrue (ml50 * times100  =? l5) ""
        }
    ]

[<Tests>]
let calculationTests =

    let (>>?) res exp =
        Expect.isTrue (res = (exp |> VU.fromString)) ""
        res


    testList "Calculation" [

        test "3 times 3 = 9" {
            Expect.isTrue (times3 * times3 |> VU.toBase = 9N) ""
        }        

        test "3 divided by 3 = 1" {
            Expect.isTrue (times3 / times3 |> VU.toBase = 1N) ""
        }        

        test "3 plus 3 = 6" {
            Expect.isTrue (times3 + times3 |> VU.toBase = 6N) ""
        }        

        test "3 minus 3 = 0" {
            Expect.isTrue (times3 - times3 |> VU.toBase = 0N) ""
        }

        test "can add or subrract within the same unit group" {
            Expect.isTrue ((l5 + ml50) >? l5) ""
            Expect.isTrue ((l5 - ml50) <? l5) ""
        }        

        test "cannot add or subrract with different unit groups" {
            Expect.throws (fun _ -> (l5 + mg400) >? l5 |> ignore) ""
            Expect.throws (fun _ -> (l5 - mg400) <? l5 |> ignore) ""
        }

        test "division by unit with the same unit group results in a count" {
            // division by a simple unit
            let (_, u) = (l5 / ml50) |> VU.get
            let g = u |> VU.Group.unitToGroup
            Expect.isTrue (g = VU.Group.CountGroup) ""

            // division by a more complex unit
            let vu1 = (mg400 / ml5 / day2) 
            let vu2 = (mg400 / l5  / hour3)

            vu1 / vu2
            |> VU.get
            |> snd
            |> VU.Group.unitToGroup
            |> equals VU.Group.CountGroup
        }

        test "can calculate with units" {
            (mg400 + mg400)
            // 400 mg + 400 mg = 800 mg
            >>? "800 mg[Mass]"
            |> (fun vu -> vu / ml50)
            // 800 mg / 50 ml = 16 mg/ml
            >>? "16 mg[Mass]/ml[Volume]"
            |> (fun vu -> vu * ml50)
            // 16 mg/ml * 50 ml = 800 mg
            >>? "800 mg[Mass]"
            // 800 mg - 400 mg = 400 mg
            |> (fun vu -> vu - mg400)
            >>? "400 mg[Mass]"
            |> ignore    
        }
    ]


[<Tests>]
let conversionTests = 

    testList "Conversion" [
        
        test "can convert from 5 liter to 5000 ml" {
            5000N |> VU.create VU.Units.Volume.milliLiter
            |> equals (l5 ==> VU.Units.Volume.milliLiter) 
        }

        test "unit group from 400 mg / day = mass per timegroup" {
            (mg400 / (1N |> VU.create VU.Units.Time.day))
            |> VU.get 
            |> snd
            |> VU.Group.unitToGroup
            |> VU.Group.toString
            |> equals "Mass/Time"
        }

        test "the number of possible units is the permutation of the units in each unitgroup" {
            // get the number of units in the mass group
            let mc = VU.Group.MassGroup   |> VU.Group.getUnits |> List.length
            // get the number of units in the volume group
            let vc = VU.Group.VolumeGroup |> VU.Group.getUnits |> List.length

            (mg400 / ml50)
            |> VU.get
            |> snd
            |> VU.Group.unitToGroup
            |> VU.Group.getUnits
            |> List.length
            // the number of units for the Mass/Volume group is 
            // the multiple of mc * vc
            |> equals (mc* vc)
        }
    
    ]