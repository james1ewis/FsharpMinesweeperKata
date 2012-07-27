// Learn more about F# at http://fsharp.net

let grid = "
*----
--*--
-*---
-----
--***
"
let width, height = 5, 5

let lines = grid.Split('\r','\n')
                |> Array.filter(fun str -> str.Length > 0)

let explodedGrid = Array2D.init width height (fun x y -> lines.[x].[y])

let computeNeighbours x y (board:char[,]) =
    let getValue (x,y) =
        let x = (x + width) % width
        let y = (y + height) % height
        board.[x,y]
    [
        -1,-1; 0,-1; 1,-1;
        -1, 0;       1, 0;
        -1, 1; 0, 1; 1, 1;  
    ]
    |> Seq.map (fun (dx,dy) -> x+dx,y+dy)
    |> Seq.map getValue
    |> Seq.sumBy (fun char -> if char = '*' then 1 else 0)

let result = explodedGrid 
                |> Array2D.mapi (fun x y n -> 
                    match n, computeNeighbours x y explodedGrid with
                        | n -> n
                   )

                