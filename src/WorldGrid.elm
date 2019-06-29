module WorldGrid exposing (State(..), emptyGrid, numberOccupied, spot, toggleState, updateCells)

import Array exposing (Array)
import CellGrid exposing (CellGrid(..), CellType(..), cellAtMatrixIndex)
import Maybe.Extra
import Random


type State
    = Crop
    | City
    | Nature
    | Unoccupied


emptyGrid : Int -> Int -> CellGrid State
emptyGrid rows cols =
    CellGrid.fromList rows cols (List.repeat (rows * cols) Unoccupied)
        |> Maybe.withDefault CellGrid.empty


updateCells : CellGrid State -> CellGrid State
updateCells cellGrid =
    cellGrid


vacate : ( Int, Int ) -> CellGrid State -> CellGrid State
vacate ( i, j ) cg =
    CellGrid.setValue cg ( i, j ) Unoccupied


toggleState : State -> ( Int, Int ) -> CellGrid State -> CellGrid State
toggleState newState ( i, j ) cg =
    case CellGrid.cellAtMatrixIndex ( i, j ) cg of
        Nothing ->
            cg

        Just state ->
            if state == newState then
                CellGrid.setValue cg ( i, j ) Unoccupied

            else
                CellGrid.setValue cg ( i, j ) newState


spot : ( Int, Int ) -> Float -> State -> CellGrid State -> CellGrid State
spot ( centerI, centerJ ) radius state cg =
    let
        cellTransformer : ( Int, Int ) -> State -> State
        cellTransformer ( i, j ) t =
            let
                di =
                    toFloat <| i - centerI

                dj =
                    toFloat <| j - centerJ
            in
            case di * di + dj * dj <= radius * radius of
                True ->
                    state

                False ->
                    t
    in
    CellGrid.mapWithIndex cellTransformer cg


gen : Int -> ( Float, Float ) -> Random.Generator (List Float)
gen n ( a, b ) =
    Random.list n (Random.float a b)


makeSeed : Int -> Random.Seed
makeSeed k =
    Random.initialSeed k


cellSequence_ : Int -> Random.Seed -> ( Float, Float ) -> ( List Float, Random.Seed )
cellSequence_ n seed ( a, b ) =
    Random.step (gen n ( a, b )) seed


neighborFilter : CellGrid u -> ( Int, Int ) -> Bool
neighborFilter (CellGrid ( nRows, nCols ) _) ( a, b ) =
    a >= 0 && a < nRows && b >= 0 && b < nCols


neighborIndices : CellGrid a -> ( Int, Int ) -> List ( Int, Int )
neighborIndices cg ( row, col ) =
    [ ( row - 1, col )
    , ( row + 1, col )
    , ( row, col - 1 )
    , ( row, col + 1 )
    , ( row - 1, col - 1 )
    , ( row - 1, col + 1 )
    , ( row + 1, col - 1 )
    , ( row + 1, col + 1 )
    ]
        |> List.filter (neighborFilter cg)


neighbors : CellGrid a -> ( Int, Int ) -> List a
neighbors cg ( row, col ) =
    neighborIndices cg ( row, col )
        |> List.map (\( r, c ) -> cellAtMatrixIndex ( r, c ) cg)
        |> Maybe.Extra.values


numberVacant : CellGrid State -> Int
numberVacant (CellGrid ( _, _ ) cells) =
    cells
        |> Array.filter (\state -> state == Unoccupied)
        |> Array.length


numberOccupied : CellGrid State -> Int
numberOccupied (CellGrid ( _, _ ) cells) =
    cells
        |> Array.filter (\state -> state /= Unoccupied)
        |> Array.length


occupiedNeighbors : CellGrid State -> ( Int, Int ) -> Int
occupiedNeighbors cg ( row, col ) =
    neighbors cg ( row, col )
        |> List.filter (\state -> state /= Unoccupied)
        |> List.length
