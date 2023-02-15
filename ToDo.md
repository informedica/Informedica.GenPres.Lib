# To Do List

* [x] Switch from float to decimal types
* [ ] Add possibility to select none assortment products
* [ ] Refactor magic values like "keer"
* [ ] Refactor setting dose values and dose units in DrugOrder
* [x] Remove calculation of units?
* [x] Improve error management and printing
* [x] Add manual prepared (i.e. no solutions for divisibility of product) generic products
* [ ] Filter parenteral meds for reconstitution and dilution
* [ ] Filter prescription rule product list
* [ ] Create nicely formatted generic product name
* [ ] Investigate amfo B lipid vs complex problem
* [ ] Fix problem with missing prescription time
* [ ] Consider merging continuous with timed
* [x] Create separate library with core domain concepts (like patient)
* [ ] Add cache management
* [ ] Identify situations with undetermined dose counts (if they exist)
* [ ] GenSolver able to detect min max calculation problems from initial equation set
* [ ] Detect "should have solution rule", but has no solution rule
* [ ] Detect "should have reconstitution", but has no reconstitution
* [ ] Solve incompatible max quantity with adjusted dose quantities
* [x] Need more testing code for solver lib
* [ ] Add unrecognized units
* [ ] Unify configuration settings and mapping in one lib
* [x] Change ValueUnit to ValuesUnit, i.e. multiple values per unit
* [ ] Differentiate between substance and shape dose
* [x] Adding continuous drip medications and rules
* [x] Improve product divisibility code, for example droplet detection
* [ ] Adding dose check with Z-index
* [ ] Differentiate between whether or not, try calculation with manual product
* [ ] Mathematically investigate whether increment calc works with addition and/or subtraction
* [ ] Maybe add abs max to constraints
* [x] Rewrite GStand.fs using anonymous records and split functions
* [ ] Split Optics modules in Optics and AutoOpen SetGet
* [x] Fix problem with selecting right solution rule depending on dose
