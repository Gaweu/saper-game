open System

type Cell =
    { mutable HasBomb: bool
      mutable Revealed: bool
      mutable Flagged: bool
      mutable AdjacentBombs: int }

type Board =
    { Size: int
      Cells: Cell[,] }

let initializeBoard size bombCount =
    let cells = Array2D.init size size (fun _ _ -> { HasBomb = false; Revealed = false; Flagged = false; AdjacentBombs = 0 })

    let random = Random()
    let rec placeBombs remainingBombs =
        if remainingBombs > 0 then
            let row = random.Next(size)
            let col = random.Next(size)
            if not cells.[row, col].HasBomb then
                cells.[row, col].HasBomb <- true
                placeBombs (remainingBombs - 1)
            else
                placeBombs remainingBombs

    placeBombs bombCount

    { Size = size; Cells = cells }

let printBoard board =
    printfn ""
    printfn "    %s" (String.replicate (board.Size * 3 + 1) "-")
    printfn "    %s" (String.concat " " [for i in 1..board.Size -> sprintf "%2d" i])
    printfn "    %s" (String.replicate (board.Size * 3 + 1) "-")
    for row in 0..(board.Size - 1) do
        printf "%2d |" (row + 1)
        for col in 0..(board.Size - 1) do
            let cell = board.Cells.[row, col]
            let value =
                match cell.Revealed with
                | true ->
                    if cell.HasBomb then "B"
                    else
                        match cell.AdjacentBombs with
                        | 0 -> " "
                        | n -> string n
                | false ->
                    if cell.Flagged then "F"
                    else "."
            let color =
                match cell.Revealed with
                | true ->
                    if cell.HasBomb then ConsoleColor.Red
                    else
                        match cell.AdjacentBombs with
                        | 1 -> ConsoleColor.Blue
                        | 2 -> ConsoleColor.Green
                        | 3 -> ConsoleColor.DarkYellow
                        | 4 -> ConsoleColor.DarkMagenta
                        | 5 -> ConsoleColor.DarkCyan
                        | _ -> ConsoleColor.White
                | false -> ConsoleColor.White
            Console.ForegroundColor <- color
            printf " %s " value
        Console.ResetColor()
        printfn "|"
    printfn "    %s" (String.replicate (board.Size * 3 + 1) "-")

let updateAdjacentBombs board =
    let directions = [(0, -1); (0, 1); (-1, 0); (1, 0); (-1, -1); (-1, 1); (1, -1); (1, 1)]
    let isWithinBounds row col =
        row >= 0 && row < board.Size && col >= 0 && col < board.Size

    for row in 0..(board.Size - 1) do
        for col in 0..(board.Size - 1) do
            if not board.Cells.[row, col].HasBomb then
                let mutable count = 0
                for (dx, dy) in directions do
                    let newRow = row + dx
                    let newCol = col + dy
                    if isWithinBounds newRow newCol && board.Cells.[newRow, newCol].HasBomb then
                        count <- count + 1
                board.Cells.[row, col].AdjacentBombs <- count

let revealEmptyCells board row col =
    let directions = [(0, -1); (0, 1); (-1, 0); (1, 0); (-1, -1); (-1, 1); (1, -1); (1, 1)]
    let isWithinBounds row col =
        row >= 0 && row < board.Size && col >= 0 && col < board.Size

    let visited = Array2D.create board.Size board.Size false

    let rec revealCell r c =
        if isWithinBounds r c && not visited.[r, c] then
            visited.[r, c] <- true
            board.Cells.[r, c].Revealed <- true
            if board.Cells.[r, c].AdjacentBombs = 0 then
                for (dx, dy) in directions do
                    let newRow = r + dx
                    let newCol = c + dy
                    revealCell newRow newCol

    revealCell row col

let areAllCellsRevealed board =
    let mutable allRevealed = true
    for row in 0..(board.Size - 1) do
        for col in 0..(board.Size - 1) do
            if not board.Cells.[row, col].Revealed && not board.Cells.[row, col].HasBomb then
                allRevealed <- false
    allRevealed

let playSaperGame size bombCount =
    let board = initializeBoard size bombCount
    updateAdjacentBombs board

    let rec gameLoop board =
        printBoard board
        printfn "Enter the row, column, and action (B - bomb, F - flag) separated by spaces: "
        let input = Console.ReadLine().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        if input.Length <> 3 then
            printfn "Invalid input. Please try again."
            gameLoop board
        else
            let validInput, row, col, action =
                match Int32.TryParse(input.[0]) with
                | true, row ->
                    match Int32.TryParse(input.[1]) with
                    | true, col ->
                        let action = input.[2].ToUpper()
                        if action = "B" || action = "F" then true, row, col, action
                        else false, -1, -1, ""
                    | _ -> false, -1, -1, ""
                | _ -> false, -1, -1, ""

            if not validInput || row < 1 || row > size || col < 1 || col > size then
                printfn "Invalid input. Please try again."
                gameLoop board
            else
                let cell = board.Cells.[row - 1, col - 1]
                if cell.Revealed then
                    printfn "Cell already revealed. Please try again."
                    gameLoop board
                elif cell.Flagged && action = "F" then
                    cell.Flagged <- false
                    gameLoop board
                elif not cell.Flagged && action = "F" then
                    cell.Flagged <- true
                    gameLoop board
                elif cell.HasBomb && action = "B" then
                    cell.Revealed <- true
                    printBoard board
                    printfn "Game over! You hit a bomb."
                else
                    cell.Revealed <- true
                    revealEmptyCells board (row - 1) (col - 1)
                    if areAllCellsRevealed board then
                        printBoard board
                        printfn "Congratulations! You won!"
                    else
                        gameLoop board

    gameLoop board

let main() =
    Console.Clear()
    printfn "Welcome to Console Minesweeper!"
    printfn "Enter the number of bombs to be placed on the board (16 to 32): "
    let bombCount = Console.ReadLine() |> int
    if bombCount < 16 || bombCount > 32 then
        printfn "Invalid input. Please enter a number between 16 and 32."
    else
        playSaperGame 16 bombCount

main()
