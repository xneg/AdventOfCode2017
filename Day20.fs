module Day20

// problem page
// http://adventofcode.com/2017/day/20

open System.IO
open System.Text.RegularExpressions

type Vector = 
        {x: int; y: int; z: int}
        member this.Manhattan = abs (this.x) + abs (this.y) + abs (this.z)
        member this.Multiply a = {x = this.x * a; y = this.y * a; z = this.z * a}
        member this.Sum a = {x = this.x + a.x; y = this.y + a.y; z = this.z + a.z}
        member this.SquareLength = this.x * this.x + this.y * this.y + this.z * this.z


type KinematicData = 
        {p : Vector; v : Vector; a : Vector}
        member this.Position (t : int) = 
                            this.a.Multiply (t * t / 2) 
                            |> (this.v.Multiply t).Sum 
                            |> this.p.Sum     
        member this.Distance (t : int) = (this.Position t).Manhattan
                                                                                                  

let processFile (filePath : string) =
        seq {
            use fileReader = new StreamReader(filePath)

            while not fileReader.EndOfStream do
                let line = fileReader.ReadLine()
                yield line
        }

let problemFileName = @"Data\20.txt"

let getItems (matchCollection : MatchCollection) = [for i in 0..matchCollection.Count - 1 -> matchCollection.Item i]

let createKinematicData (data : int array) =
        {p = {x = data.[0]; y = data.[1]; z = data.[2]};
         v = {x = data.[3]; y = data.[4]; z = data.[5]};
         a = {x = data.[6]; y = data.[7]; z = data.[8]}}    

let data = problemFileName 
           |> processFile
           |> Seq.map (fun x -> Regex.Matches (x, @"[0-9\-]+") 
                                |> getItems 
                                |> List.map (fun x -> (int)x.Value) |> List.toArray |> createKinematicData)

let result = data |> Seq.map (fun x -> x.a.Manhattan * 1000 + x.v.Manhattan) |> Seq.mapi (fun i v -> i, v) |> Seq.minBy snd

//2nd part

let rec collide t stop list =
        if t < stop then
                let duplicates = list |> Seq.groupBy (fun (x : KinematicData) -> x.Position 1)
                let list =      list 
                                |> Seq.map (fun (x : KinematicData) -> {p = x.Position 1; v = x.v; a = x.a}) 
                                |> Seq.distinctBy (fun x -> x.p)
                collide (t + 1) stop list
        else
                list |> Seq.length        

data |> collide 0 20000

let testResult = @"Data\20_test.txt" 
                   |> processFile
                   |> Seq.map (fun x -> Regex.Matches (x, @"[0-9\-]+") 
                                        |> getItems 
                                        |> List.map (fun x -> (int)x.Value) |> List.toArray |> createKinematicData)
                   |> collide 0 2