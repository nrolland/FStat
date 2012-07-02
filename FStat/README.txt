

- To install to a new place, after git checkout, run
   nuget i .\FStat\packages.config -o Packages

- When references are changed, run Setup.fsx
  Setup.fsx generates in each project directory a file named __solmerged.fsx.
  It contains all the reference used by the solution, written with a path to the project the copy you look at is copied.
  After referencing this file in a script, it will contain the same typespace as you code in VS.
  That means that your script compiles in VS, it should works in fsi.

- 