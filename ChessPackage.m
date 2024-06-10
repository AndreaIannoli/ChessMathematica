(* ::Package:: *)

(* :Title: ChessMathematica *)
(* :Context: ChessPackage` *)
(* :Author: A. Iannoli, V. Rossetto, G. Centonze *)
(* :Summary: an inclusive chess player *)
(* :Copyright: Iannoli Rossetto Centonze 2024 *)
(* :Package Version: 1.5 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 20/5/2024 *)
(* :Keywords: programming style, local variables, chess, mathematica, inclusive, accessability *)
(* :Sources: biblio *)
(* :Limitations: this is for educational purposes only. *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: package Context is not defined *)

BeginPackage["ChessPackage`"]

plotGui::usage = "plotGui[] plot the gui";
i::usage ="the index of the manipulate"; (*The variable 'i' used in 'manipulate' represents the index of the move being displayed. It must be exported unless you want to see the package name next to the variable 'i' in 'manipulate'. Since the package is private, Mathematica specifies it. By removing this line of code, everything will work correctly, but you will see the package name next to the 'manipulate' variable.*)


Begin["`Private`"]
(* Decomment to print the list of imported functions on main*)
(*Print["List of Functions"];
Print["\tplotGui"];*)


(* Define the package directory based on the full path of the file *)
packageDir = $InputFileName; 


(*FROM HERE UNTIL THE NEXT COMMENT IN UPPERCASE THAT SPECIFIES IT, THE CODE IS FROM THE CHESS LIBRARY. WE ALSO MADE A FEW SMALL CHANGES IN THE FOLLOWING PART OF THE LIBRARY, BUT IT IS ONLY A FEW LINES OF CODE, THE COMMENTS ANYWAY, ARE COMPLETELY WRITTEN BY US.*)

(* Declare an array of chess piece names (short versions) *)
pieceNamesShort = {"King", "Queen", "Rook", "Bishop", "Knight", "Pawn"};

(* Create a list of white chess piece names by prepending "White" to each piece name *)
wpn = Table["White" <> pieceName, {pieceName, pieceNamesShort}];

(* Create a list of black chess piece names by prepending "Black" to each piece name *)
bpn = Table["Black" <> pieceName, {pieceName, pieceNamesShort}];

(* Combine the lists of white and black piece names into a single list *)
pieceNames = wpn ~Join~ bpn;

(* Append "EmptySquare" to the combined list to represent an empty square on the chessboard *)
AppendTo[pieceNames, "EmptySquare"];

(* Declare the symbols for white chess pieces *)
whitePieces = {"\[WhiteKing]", "\[WhiteQueen]", "\[WhiteRook]", "\[WhiteBishop]", "\[WhiteKnight]", "\[WhitePawn]"};

(* Declare the symbols for black chess pieces *)
blackPieces = {"\[BlackKing]", "\[BlackQueen]", "\[BlackRook]", "\[BlackBishop]", "\[BlackKnight]", "\[BlackPawn]"};

(* Combine the symbols for white and black pieces and add the symbol for an empty square *)
pieceSymbols = whitePieces ~Join~ blackPieces ~Join~ {"\[EmptySquare]"};

(* Assign the symbols to individual variables for easy reference *)
{\[WhiteKing], \[WhiteQueen], \[WhiteRook], \[WhiteBishop], \[WhiteKnight], \[WhitePawn], \[BlackKing], \[BlackQueen], \[BlackRook], \[BlackBishop], \[BlackKnight], \[BlackPawn], \[EmptySquare]} = pieceSymbols;

(* Note: The following line is commented out. If uncommented, it would assign the names to the symbols. *)
(*{\[WhiteKing], \[WhiteQueen], \[WhiteRook], \[WhiteBishop], \[WhiteKnight], \[WhitePawn], \[BlackKing], \[BlackQueen], \[BlackRook], \[BlackBishop], \[BlackKnight], \[BlackPawn], \[EmptySquare]} = pieceNames;*)

(* Check if the piece images file exists in the package directory *)
If[
    FileExistsQ[packageDir <> "pieceImages.mx"], (* If the file exists *)
    pieceImages = Import[packageDir <> "pieceImages.mx"]; (* Import the piece images from the file *)
    (*Print["Chess piece images were loaded."]; Commented out print statement for loading message *)
    ,
    (* If the file does not exist, proceed to download the images *)

    (* Create an array with links to the piece images from Wikimedia *)
    piecePicLinksWiki = {
        "https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Chess_klt45.svg/75px-Chess_klt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Chess_qlt45.svg/75px-Chess_qlt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/72/Chess_rlt45.svg/75px-Chess_rlt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b1/Chess_blt45.svg/75px-Chess_blt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/7/70/Chess_nlt45.svg/75px-Chess_nlt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Chess_plt45.svg/75px-Chess_plt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f0/Chess_kdt45.svg/75px-Chess_plt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/4/47/Chess_qdt45.svg/75px-Chess_qdt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/f/ff/Chess_rdt45.svg/75px-Chess_rdt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/Chess_bdt45.svg/75px-Chess_bdt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/Chess_ndt45.svg/75px-Chess_ndt45.svg.png",
        "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Chess_pdt45.svg/75px-Chess_pdt45.svg.png"
    };

    (* Base URL for another set of chess piece images *)
    imagePicMotherLink = "https://marcelk.net/chess/pieces/merida/320/";

    (* Create an array of full URLs to the piece images *)
    piecePicLinks = Table["https://marcelk.net/chess/pieces/merida/320/" <> pieceName <> ".png", {pieceName, Most @ pieceNames}];

    (* Import the piece images from the Wikimedia links *)
    pieceImages = Import /@ piecePicLinksWiki;

    (* Append a graphical representation of an empty square to the piece images array *)
    AppendTo[pieceImages, Graphics[{Hue[0, 0, 1, 0], Rectangle[]}, ImageSize -> ImageDimensions @ First @ pieceImages]];

    (* Export the imported piece images to a .mx file for future use *)
    Export[packageDir <> "pieceImages.mx", pieceImages];
    
    (*Note on the mx format and why we use it:
    The MX Mathematica format is a proprietary binary file format used by Wolfram Mathematica for efficiently storing and exchanging data between Mathematica sessions.
    In summary, the MX Mathematica format is used for caching images due to its efficient, fast, and compact nature, making it ideal for preserving and quickly accessing (with a built-in function) complex graphical data within the Mathematica environment.*)

    (* Print a message indicating that the piece images have been downloaded *)
    Print["Chess piece images were downloaded."]
]


(* Define an association `pieceObj` that maps each piece symbol to its corresponding name, symbol, and image. *)
pieceObj = 
	MapThread[
		#1 -> <|"Name" -> #2, "Symbol" -> #3, "Image" -> #4|>&,
		{pieceSymbols, pieceNames, pieceSymbols, pieceImages}
	];

(* Convert the list of rules to an association for easy lookup *)	
pieceObj = Association @ pieceObj;

(* Define the columns and rows of the chessboard. *)
files = {"a", "b", "c", "d", "e", "f", "g", "h"};  (* Chessboard columns. *)
fileSize = Length @ files; (* Number of columns. *)
ranks = {"1", "2", "3", "4", "5", "6", "7", "8"}; (* Chessboard rows. *)
rankSize = Length @ ranks; (* Number of rows. *)

(* Generate chessboard pattern *)
emptyBoard = Table[Mod[i + j, 2], {i, 8}, {j, 8}]; (* Standard empty chessboard pattern *)
bottomLeftBlackBoard = Table[Mod[i + j    , 2], {i, 8}, {j, 8}]; (* Board with bottom-left square black *)
bottomLeftWhiteBoard = Table[Mod[i + j + 1, 2], {i, 8}, {j, 8}]; (* Board with bottom-left square white *)

(* Define the Black Coordinate Matrix (BCM) which maps chessboard positions from Black's perspective. *)
bcm = \!\(\*
TagBox[
RowBox[{"(", GridBox[{
{"\"\<a8\>\"", "\"\<b8\>\"", "\"\<c8\>\"", "\"\<d8\>\"", "\"\<e8\>\"", "\"\<f8\>\"", "\"\<g8\>\"", "\"\<h8\>\""},
{"\"\<a7\>\"", "\"\<b7\>\"", "\"\<c7\>\"", "\"\<d7\>\"", "\"\<e7\>\"", "\"\<f7\>\"", "\"\<g7\>\"", "\"\<h7\>\""},
{"\"\<a6\>\"", "\"\<b6\>\"", "\"\<c6\>\"", "\"\<d6\>\"", "\"\<e6\>\"", "\"\<f6\>\"", "\"\<g6\>\"", "\"\<h6\>\""},
{"\"\<a5\>\"", "\"\<b5\>\"", "\"\<c5\>\"", "\"\<d5\>\"", "\"\<e5\>\"", "\"\<f5\>\"", "\"\<g5\>\"", "\"\<h5\>\""},
{"\"\<a4\>\"", "\"\<b4\>\"", "\"\<c4\>\"", "\"\<d4\>\"", "\"\<e4\>\"", "\"\<f4\>\"", "\"\<g4\>\"", "\"\<h4\>\""},
{"\"\<a3\>\"", "\"\<b3\>\"", "\"\<c3\>\"", "\"\<d3\>\"", "\"\<e3\>\"", "\"\<f3\>\"", "\"\<g3\>\"", "\"\<h3\>\""},
{"\"\<a2\>\"", "\"\<b2\>\"", "\"\<c2\>\"", "\"\<d2\>\"", "\"\<e2\>\"", "\"\<f2\>\"", "\"\<g2\>\"", "\"\<h2\>\""},
{"\"\<a1\>\"", "\"\<b1\>\"", "\"\<c1\>\"", "\"\<d1\>\"", "\"\<e1\>\"", "\"\<f1\>\"", "\"\<g1\>\"", "\"\<h1\>\""}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}], ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);
(* Define the White Coordinate Matrix (WCM) by reversing BCM, so it's from White's perspective. *)
wcm = Reverse[bcm];

(* Initial state of the chessboard with Unicode characters representing pieces and empty squares. *)
stateMatrix0 = \!\(\*
TagBox[
RowBox[{"(", GridBox[{
{"\[BlackRook]", "\[BlackKnight]", "\[BlackBishop]", "\[BlackQueen]", "\[BlackKing]", "\[BlackBishop]", "\[BlackKnight]", "\[BlackRook]"},
{"\[BlackPawn]", "\[BlackPawn]", "\[BlackPawn]", "\[BlackPawn]", "\[BlackPawn]", "\[BlackPawn]", "\[BlackPawn]", "\[BlackPawn]"},
{"\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]"},
{"\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]"},
{"\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]"},
{"\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]", "\[EmptySquare]"},
{"\[WhitePawn]", "\[WhitePawn]", "\[WhitePawn]", "\[WhitePawn]", "\[WhitePawn]", "\[WhitePawn]", "\[WhitePawn]", "\[WhitePawn]"},
{"\[WhiteRook]", "\[WhiteKnight]", "\[WhiteBishop]", "\[WhiteQueen]", "\[WhiteKing]", "\[WhiteBishop]", "\[WhiteKnight]", "\[WhiteRook]"}
},
GridBoxAlignment->{"Columns" -> {{Center}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.7]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}], ")"}],
Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]\);


(* Create an initial chess state list that maps each position on the board to its initial state (piece or empty square). *)
initialChessStateList = Thread[Flatten[bcm] -> Flatten[stateMatrix0]];

(* Add special chess state properties to the initial state list. *)
initialChessStateList = initialChessStateList ~Join~ {"\[WhiteRook]\[WhiteKing]" -> True, "\[WhiteKing]\[WhiteRook]" -> True, "\[BlackRook]\[BlackKing]" -> True, "\[BlackKing]\[BlackRook]" -> True, "EnPassant" -> None}; (* Castling rights. And no en passant target square initially.*)

(* Append additional state properties to the initial state list. *)
AppendTo[initialChessStateList, "WhiteCemetery" -> {}]; (* List of captured white pieces. *)
AppendTo[initialChessStateList, "BlackCemetery" -> {}];
AppendTo[initialChessStateList, "Turn" -> "White"]; (* White's turn initially. *)
AppendTo[initialChessStateList, "Result" -> {}]; (* No game result initially. *)
(* Display the initial chess state list. *)
initialChessStateList;

(* Define a function to get the state of a position on the chessboard. *)
ChessState[a_?AssociationQ][pos_?StringQ] /; StringMatchQ[pos, files ~~ ranks] := 
	Lookup[a, pos, \[EmptySquare]] (* Return the piece at the position, or empty square if none. *)

(* Define a function to get the state using row and column indices. *)	
ChessState[a_?AssociationQ][i_Integer, j_Integer] := 
	ChessState[a][files[[i]] <> ranks[[j]]] (* Convert indices to position string and get state. *)
	
(* Define a function to get the state using a list of row and column indices. *)
ChessState[a_?AssociationQ][{i_Integer, j_Integer}] := 
	ChessState[a][i, j] (* Delegate to the previous function. *)

(* Define a custom display format for a ChessState object. *)
ChessState /: MakeBoxes[cs_ChessState, StandardForm] := 
	Module[{icon},
		icon = 
			ToBoxes[
				ImageResize[
					 (* Use Rasterize to create an image of the chessboard. *)
					Rasterize[ChessPlot[cs, "MatrixForm"-> True]]
					, 
					300
				]
		];
		
		(* Create an interpretation box for the ChessState object. *)	
		InterpretationBox @@ 
			{
				RowBox[{"ChessState", "[", icon, "]"}],
			cs
			}
	]
	
(* Initialize the chess state using the initial chess state list. *)
InitializedChessState = ChessState[Association@initialChessStateList];

(* Define a function to create a table of piece images on the chessboard. *)
tabulatePieceInset[insetToAlgFunc_, chessState_] :=
	Table[
		Inset[
			(* Get the image of the piece at the given position. *)
			pieceObj[chessState[insetToAlgFunc[i, j]]]["Image"], 
			{i, j} - {1, 1}, (* Position to place the piece image on the board. *)
			{i, j} - {1, 1}, (* Alignment of the piece image. *)
			1 (* Scale factor for the piece image. *)
		],
		{i, 8},  (* Loop over columns (1 to 8). *)
		{j, 8} (* Loop over rows (1 to 8). *)
	];
	
(* Define a function to create a table of coordinate labels on the chessboard. *)		
tabulateCoordInset[insetToAlgFunc_] := 
	Table[
		Inset[
			(* Create a text label for the coordinate. *)
			Text[Style[insetToAlgFunc[i, j], 7]], 
			{i, j} + {-0.12, -0.09}, (* Position to place the text label. *)
			Automatic, (* Automatic alignment *)
			1 (* Scale factor for the text label. *)
		], (* Iterate over rows and columns *)
		{i, 8},
		{j, 8}
	];
		

(* Coordinates when White is at the bottom (default orientation). *)
whiteDownCoordTable = 
	tabulateCoordInset[{i, j} |-> files[[i]] <> ranks[[j]]];
(* Coordinates when White is at the top. *)
whiteUpCoordTable = 
	tabulateCoordInset[{i, j} |-> files[[9 - i]] <> ranks[[9 - j]]];
(* Coordinates when White is on the left. *)
whiteLeftCoordTable = 
	tabulateCoordInset[{i, j} |-> files[[9 - j]] <> ranks[[i]]];
(* Coordinates when White is on the right. *)
whiteRightCoordTable =
	tabulateCoordInset[{i, j} |-> files[[j]] <> ranks[[9 - i]]];
(* We only used default orientation in our project. *)
				
(* Define default options for the ChessPlot function. *)
Options[ChessPlot] = Join[
    {
        "WhiteOrientation" -> Automatic,  (* Orientation of the board, default is Automatic. *)
        "BoardColorSet" -> Automatic,     (* Colors of the board squares, default is Automatic. *)
        "ShowCoordinates" -> True,        (* Whether to show coordinates, default is True. *)
        "MatrixForm" -> False             (* Whether to display the board as a matrix, default is False. *)
    },
    Options[ArrayPlot]                    (* Include options from ArrayPlot for customization. *)
];

(* Define the ChessPlot function to visualize the chess state. *)
ChessPlot[chessState_, opts : OptionsPattern[]] :=    
    Module[
        {
            cs = First[chessState], pieceInset, coordInset, insetList, emptyBoard, plot, LightSquareColor, DarkSquareColor
        },
        
        (* Handle the "MatrixForm" option. If True, return a MatrixForm of the chess state. *)
        If[OptionValue["MatrixForm"], 
            Return[
                MatrixForm[Partition[Values[cs[[ ;; rankSize fileSize]]], Length[files]]]
            ]
        ];
        
        (* Set up piece and coordinate insets based on the "WhiteOrientation" option. *)
        Switch[
            OptionValue["WhiteOrientation"],
            Automatic | "Down",  (* Default orientation or White at the bottom. *)
                pieceInset = tabulatePieceInset[{i, j} |-> files[[i]] <> ranks[[j]], cs];  (* Pieces placed normally. *)
                coordInset = whiteDownCoordTable;  (* Coordinates shown with White at the bottom. *)
                emptyBoard = bottomLeftBlackBoard;  (* Standard empty board pattern. *)
            ,
            "Up",  (* White at the top. *)
                pieceInset = tabulatePieceInset[{i, j} |-> files[[9 - i]] <> ranks[[9 - j]], cs];  (* Pieces flipped vertically. *)
                coordInset = whiteUpCoordTable;  (* Coordinates shown with White at the top. *)
                emptyBoard = bottomLeftBlackBoard;  (* Standard empty board pattern. *)
            ,
            "Left",  (* White on the left. *)
                pieceInset = tabulatePieceInset[{i, j} |-> files[[9 - j]] <> ranks[[i]], cs];  (* Pieces rotated 90 degrees counterclockwise. *)
                coordInset = whiteLeftCoordTable;  (* Coordinates shown with White on the left. *)
                emptyBoard = bottomLeftWhiteBoard;  (* Board pattern adjusted for left orientation. *)
            ,
            "Right",  (* White on the right. *)
                pieceInset = tabulatePieceInset[{i, j} |-> files[[j]] <> ranks[[9 - i]], cs];  (* Pieces rotated 90 degrees clockwise. *)
                coordInset = whiteRightCoordTable;  (* Coordinates shown with White on the right. *)
                emptyBoard = bottomLeftWhiteBoard;  (* Board pattern adjusted for right orientation. *)
            ,
            _,  (* Handle invalid orientation options. *)
                Print["Wrong \"WhiteOrientation\" option was provided. Try \"Up\" for instance."]; Return[$Failed];
        ];
        
        (* Set up the board colors based on the "BoardColorSet" option. *)
        Switch[
            OptionValue["BoardColorSet"],
            Automatic,  (* Default colors for the board. *)
                LightSquareColor = RGBColor[1.0, 0.8, 0.6];  (* Light square color. *)
                DarkSquareColor  = RGBColor[0.8, 0.5, 0.3];  (* Dark square color. *)
            ,
            {_?ColorQ, _?ColorQ},  (* Custom colors for the board. *)
                {LightSquareColor, DarkSquareColor} = OptionValue["BoardColorSet"];
            ,
            _,  (* Handle invalid color options. *)
                Print["Wrong \"BoardColorSet\" Specified. Try {White, Gray} for instance."]; Return[$Failed];
        ];
        
        (* Set up the insets list based on the "ShowCoordinates" option. *)
        Switch[
            OptionValue["ShowCoordinates"],
            True,  (* Show coordinates. *)
                insetList = pieceInset ~Join~ coordInset;
            ,
            False,  (* Do not show coordinates. *)
                insetList = pieceInset ~Join~ {};
            ,
            _,  (* Handle invalid options for showing coordinates. *)
                Print["\"ShowCoordinates\" can either be True or False."]; Return[$Failed];
        ];
        
        (* Create the chessboard plot using ArrayPlot. *)
        plot =         
            ArrayPlot[
                emptyBoard,
                ColorRules -> {0 -> LightSquareColor, 1 -> DarkSquareColor},  (* Assign colors to the squares. *)
                Epilog -> insetList,  (* Add piece and coordinate insets. *)
                FilterRules[{opts}, Options[ArrayPlot]]  (* Apply additional ArrayPlot options. *)
            ];
        
        plot
    ]

	
(* Define the map functions shortcuts for ChessPlot with different options*)
mp = ChessPlot[#, "MatrixForm" -> True]&;
fp = ChessPlot[#, "MatrixForm" -> False]&;

(* Function to check if a position is within the valid range of the chessboard *)
inRangeQ[pos_] :=
	Module[{cond1, cond2},
	cond1 = (1 <= pos[[1]] <= rankSize); (* Check row index *)
	cond2 = (1 <= pos[[2]] <= fileSize); (* Check column index *)
	Return[(cond1 \[And] cond2)]; (* Return True if both conditions are met, otherwise False *)
	]

(* Define function B1 to calculate possible moves for pawns *)
(* This is the only piece moving function that is going to be commente deeply, 
as the other functions for piece's movement work basically the same but with diffenrent conditions *)
B1[ch_] :=
	Module[{S = First[Last[ch]], actions, turn, piece, pos1List, coordMatrix, pos1, pos2, pos3, coord2, cond1, cond2, act1, act2},
		actions = {}; (* Initialize the list of actions *)
		
		(* Determine whose turn it is *)
		turn = Mod[Length@ch + 1, 2] + 1; (* 1 for White, 2 for Black *)
		piece = Part[{\[WhitePawn], \[BlackPawn]}, turn]; (* Select the correct pawn based on the turn *)
		coordMatrix = Part[{wcm, bcm}, turn]; (* Select the correct coordinate matrix based on the turn *)
		
		(* Find the positions of all pawns of the current player *)
		pos1List = Position[S, piece][[All, 1, 1]];
		
		(* Iterate over all pawn positions *)
		Do[
			pos1 = Position[coordMatrix, coord1]//First; (* Find the position of the current pawn in the coordinate matrix *)
			
			pos2  = pos1 + {1, 0}; (* Calculate the position in front of the pawn *)
			If[!inRangeQ[pos2], Break[];]; (* If the position is out of range, exit the loop *)
			coord2 = Extract[coordMatrix, pos2]; (* Get the coordinate string for the position in front *)
			
			(* Check if the pawn is at the last rank *)
			cond1 = pos1[[1]] == rankSize;
			(* Check if the square in front is empty *)
			cond2 = S[coord2] == \[EmptySquare];
			
			(* Define the move actions *)
			act1 = (coord1 -> \[EmptySquare]); (* Move pawn from current position *)
			act2 = (coord2 -> piece); (* Move pawn to the new position *)
			
			(* Add actions if the pawnconditions are satisfied *)
			If[!cond1 \[And] cond2, 
				AppendTo[actions, {act1, act2}];
			];
			
			(* Handle pawn promotion if the pawn is at the last rank *)		
			If[cond1 \[And] cond2, 
				AppendTo[actions, {coord1 -> \[EmptySquare], coord1 -> Part[{\[WhiteQueen], \[BlackQueen]}, turn]}];
				AppendTo[actions, {coord1 -> \[EmptySquare], coord1 -> Part[{\[WhiteRook], \[BlackRook]}, turn]}];
				AppendTo[actions, {coord1 -> \[EmptySquare], coord1 -> Part[{\[WhiteBishop], \[BlackBishop]}, turn]}];
				AppendTo[actions, {coord1 -> \[EmptySquare], coord1 -> Part[{\[WhiteKnight], \[BlackKnight]}, turn]}];
			];
			,
			{coord1, pos1List} (* Iterate over each pawn position *)
		];
		(* Remove duplicate actions *)
		actions = DeleteDuplicates @ actions;
		Return[actions]; (* Return the list of possible actions *)
	]

(* As B1 but for pawn's double step *)
B2[ch_] :=
	Module[{S = First[Last[ch]], actions, turn, piece, pos1List, coordMatrix, pos1, pos2, pos3, coord2, coord3, cond1, cond2, cond3, act1, act2},
		actions = {};
		(* Whose turn is it? *)
		turn = Mod[Length@ch + 1, 2] + 1;
		piece = Part[{\[WhitePawn], \[BlackPawn]}, turn];
		coordMatrix = Part[{wcm, bcm}, turn];
		
		pos1List = Position[S, piece][[All, 1, 1]];
		
		Do[
			pos1 = Position[coordMatrix, coord1]//First;
			
			pos2  = pos1 + {+1, 0};
			coord2 = Extract[coordMatrix, pos2];
			
			pos3  = pos2 + {+1, 0};
			coord3 = Extract[coordMatrix, pos3];
			
			(* Must be at rank 2 *)
			cond1 = pos1[[1]] == 2;
			(* The two squares in front of it must be empty *)
			cond2 = S[coord2] == \[EmptySquare];
			cond3 = S[coord3] == \[EmptySquare];
			
			act1 = (coord1 -> \[EmptySquare]);
			act2 = (coord3 -> piece);
			
			If[cond1 \[And] cond2 \[And] cond3, 
				AppendTo[actions, {act1, act2}];
			];
			,
			{coord1, pos1List}
		];
		actions = DeleteDuplicates @ actions;
		Return[actions];
	]

(* Same but pawn is eating *)
B3[ch_] :=
	Module[{S = First[Last[ch]], actions, turn, piece, pos1List, steps, coordMatrix, pos1, pos2, pos3, coord2, coord3, cond1, cond2, opponentCemetery, opponentPieces},
		actions = {};
		(* Whose turn is it? *)
		turn = Mod[Length@ch + 1, 2] + 1;
		piece = Part[{\[WhitePawn], \[BlackPawn]}, turn];
		
		(* Extract opponent's pieces and cemetery *)
		opponentPieces = Part[{Rest @ blackPieces, Rest @ whitePieces}, turn];
		opponentCemetery =  Part[{"blackCemetery", "whiteCemetery"}, turn];
		coordMatrix = Part[{wcm, bcm}, turn]; (* Select the correct coordinate matrix based on the turn *)
		
		pos1List = Position[S, piece][[All, 1, 1]];
		steps = {{1, -1}, {1, +1}}; (* Possible step directions for capturing *)
		
		Do[
			pos1 = Position[coordMatrix, coord1]//First;
			Do[
				pos2  = pos1 + step;
				If[!inRangeQ[pos2], Continue[];];
				coord2 = Extract[coordMatrix, pos2]; (* Get the coordinate string for the potential capture position *)
				
				(* Check if the potential capture position is at the last rank *)
				cond1 = pos2[[1]] == rankSize;
				
				(* Check if the piece at the potential capture position is an opponent's piece *)
				cond2 = MemberQ[opponentPieces, S[coord2]];
				
				(* Add the en passant capture action if only condition2 is met *)
				If[!cond1 \[And] cond2, 
					AppendTo[actions, {coord1 -> \[EmptySquare], S[coord2] -> opponentCemetery, coord2 -> piece}];
				];
				
				(* Handle pawn promotion for en passant captures at the last rank *)			
				If[cond1 \[And] cond2, 
					AppendTo[actions, {coord1 -> \[EmptySquare], S[coord2] -> opponentCemetery, coord2 -> Part[{\[WhiteQueen], \[BlackQueen]}, turn]}];
					AppendTo[actions, {coord1 -> \[EmptySquare], S[coord2] -> opponentCemetery, coord2 -> Part[{\[WhiteRook], \[BlackRook]}, turn]}];
					AppendTo[actions, {coord1 -> \[EmptySquare], S[coord2] -> opponentCemetery, coord2 -> Part[{\[WhiteBishop], \[BlackBishop]}, turn]}];
					AppendTo[actions, {coord1 -> \[EmptySquare], S[coord2] -> opponentCemetery, coord2 -> Part[{\[WhiteKnight], \[BlackKnight]}, turn]}];
				];
			,
			{step, steps}
			];
			,
			{coord1, pos1List}
		];
		actions = DeleteDuplicates @ actions;
		Return[actions];
	]

(* Define function KnightMoves to calculate possible moves for knights *)
KnightMoves[ch_] :=
	Module[
		{S = First[Last[ch]], actions, turn, piece, pos1List, steps, coordMatrix, pos1, pos2, pos3, coord2, coord3, cond1, cond2, opponentCemetery, opponentPieces},
		actions = {};
		
		turn = Mod[Length@ch + 1, 2] + 1;
		piece = Part[{\[WhiteKnight], \[BlackKnight]}, turn];
				
		opponentPieces = Part[{Rest @ blackPieces, Rest @ whitePieces}, turn];
		opponentCemetery =  Part[{"blackCemetery", "whiteCemetery"}, turn];
		coordMatrix = Part[{wcm, bcm}, turn];
		
		(* Find the positions of all knights of the current player *)
		pos1List = Position[S, piece][[All, 1, 1]];
		(* All possible knight moves *)
		steps = {{-2, -1}, {-2, +1}, {-1, -2}, {-1, +2}, {+1, -2}, {+1, +2}, {+2, -1}, {+2, +1}};
		
		Do[
			pos1 = Position[coordMatrix, coord1]//First;
			Do[
				pos2  = pos1 + step;
				If[!inRangeQ[pos2], Continue[];]; (* Skip if the position is out of range *)
				coord2 = Extract[coordMatrix, pos2];
				
				(* Check if the potential move position is either empty or contains an opponent's piece *)
				cond1 = MemberQ[opponentPieces ~Join~ {\[EmptySquare]}, S[coord2]];

				(* Add the action for moving to an empty square *)
				If[cond1 \[And] S[coord2] == \[EmptySquare], 
					AppendTo[actions, {coord1 -> \[EmptySquare], coord2 -> piece}];
				];
				(* Add the action for capturing an opponent's piece *)
				If[cond1 \[And] S[coord2] != \[EmptySquare],
					AppendTo[actions, {coord1 -> \[EmptySquare], S[coord2] -> opponentCemetery, coord2 -> piece}];
				]
				,
				{step, steps}
			];
			,
			{coord1, pos1List}
		];
		actions = DeleteDuplicates @ actions;
		Return[actions];
	]

(* Function to update the state of the board and append it to chessHistory, then returns chessHistory *)
ChessEvolve[chessHistory_, actions:{__Rule}] := 
	Module[{S = First[Last[chessHistory]]}, (* Extract the latest state *)
		Do[
			S[action[[1]]] = action[[2]]; (* Apply each action to the state *)
			,
			{action, actions} (* Iterate over each action *)
		];
		Return[chessHistory ~Join~ {ChessState[S]}]; (* Append the updated state to the history *)
	]


(* Definition of ChessMove function *)
ChessMove[cm : __Rule][str_?StringQ] := 
	Switch[str,
		"From", (* If str is "From", return the first element of cm (source square of the move) *)
			cm[[1]]
		,
		"To", (* If str is "To", return the second element of cm (destination square of the move) *)
			cm[[2]]
		,
		_, (* If str is neither "From" nor "To", print an error message and return $Failed *)
			Print["Try \"From\" or \"To\"."]; Return[$Failed];
	]

(* Define the display format for ChessMove objects *)
ChessMove /: MakeBoxes[cm : ChessMove[__Rule], StandardForm] := 
	Module[{icon},
		(* Create an icon representing the ChessMove object using a black bishop *)
		icon = 
			ToBoxes[
				ImageResize[
					pieceObj[\[BlackBishop]]["Image"]
					, 
					150
				]
		];
		
		(* Define the display format as an InterpretationBox containing the icon *)	
		InterpretationBox @@ 
			{
				RowBox[{"ChessMove", "[", icon, "]"}], 
			cm (* Interpretation is the original ChessMove object *)
			}
	]


(* Definition of ChessPlay function *)
ChessPlay[chessHistory_, ChessMoves:{__ChessMove}] := 
	Module[{history = chessHistory, state, piece1, piece2},
		Do[
			(* Get the current state of the game *)
			state   = First[history[[-1]]];
		
			(* Get the piece at the 'From' square and the piece at the 'To' square of the move *)
			piece1  = state[move["From"]];
			piece2  = state[move["To"]];
			
			(* Check if the piece at 'To' square is a white piece and move it to WhiteCemetery if true *)
			If[MemberQ[{\[WhiteKing], \[WhiteQueen], \[WhiteRook], \[WhiteBishop], \[WhiteKnight], \[WhitePawn]}, piece2], AppendTo[state["WhiteCemetery"], piece2]];
			(* Same for black *)
			If[MemberQ[{\[BlackKing], \[BlackQueen], \[BlackRook], \[BlackBishop], \[BlackKnight], \[BlackPawn]}, piece2], AppendTo[state["BlackCemetery"], piece2]];
						
			(* Update the 'From' square to be an empty square and move the piece to the 'To' square *)
			state[move["From"]] = \[EmptySquare];
			state[move["To"]] = piece1;
			
			(* Append the updated state to the history *)
			AppendTo[history, ChessState[state]];
			,
			{move, ChessMoves} (* Iterate through each move in ChessMoves *)
		];
		history (* Return the updated game history *)
	]


(*FROM HERE ONWARDS, THE CODE IS ALL OURS, WHILE BEFORE THIS POINT, IT BELONGS TO THE LIBRARY, TO WHICH WE STILL HAD TO MAKE SOME CHANGES.*)

cs0 = InitializedChessState;
(* Method for random game selection *)
randomGame[data_,dimensioneData_]:= Module[
{index,coordmoves,pgnmoves, chessHistory, pgnMovesArray, name, typeofwin},
index=RandomInteger[{1,dimensioneData}]; (* Randomly select an index *)
	(* Extract coordinate moves and PGN notation moves from data *)
	coordmoves=ImportString[data[[index,"processed_moves"]],"CSV"]; 
	pgnmoves=data[[index,"moves"]];
	pgnMovesArray = StringSplit[pgnmoves, " "];  (* Split PGN notation into an array of moves *)
	(* Extract the opening name and the type of win from data *)
	name = data[[index,"opening_name"]]; 
	typeofwin = data[[index,"victory_status"]];
convertToRules[coordmoves_]:=Thread[Rule@@@coordmoves];   (* Convert coordinate moves to rules for ChessMove *)
moves = ChessMove/@convertToRules[coordmoves]; (* Create ChessMove objects *)
chessHistory = ChessPlay[{cs0}, moves];   (* Generate the game with initial state cs0 (Initialized board) and the extracted moves *)
	{chessHistory, pgnMovesArray, pgnmoves, name, index, typeofwin}  (* Return game history, PGN move array, and PGN moves *)
	]
	
(* Method for given game with id "index" selection *)
(* This method does the same thing as random game, just using the index recieved as input*)
selectedGame[data_, index_]:= Module[{coordmoves,pgnmoves, chessHistory, pgnMovesArray, name, typeofwin},
	coordmoves=ImportString[data[[index,"processed_moves"]],"CSV"];   
	pgnmoves=data[[index,"moves"]];
	pgnMovesArray = StringSplit[pgnmoves, " "];  (* Split PGN notation into an array of moves *)
	name = data[[index,"opening_name"]];
	typeofwin = data[[index,"victory_status"]];
convertToRules[coordmoves_]:=Thread[Rule@@@coordmoves];   (* Convert coordinate moves to rules for ChessMove *)
moves = ChessMove/@convertToRules[coordmoves]; (* Create ChessMove objects *)
chessHistory = ChessPlay[{cs0}, moves];   (* Generate the game with initial state cs0 (Initialized board) and the extracted moves *)
	{chessHistory, pgnMovesArray, pgnmoves, name, index, typeofwin}  (* Return game history, PGN move array, and PGN moves *)
	]

(*Import games dataset from the CSV file*)
data=Import[FileNameJoin[{NotebookDirectory[],"gamesProcessed.csv"}],"Dataset","HeaderLines"->1];
(*Determine the number of rows in the imported dataset*)
dimensioneData=Length[data];

(* We use the function selectedGame to extract an initial game *)
{chessHistory, pgnMovesArray,pgnmoves, name, selectedId, typeofwin}=selectedGame[data,29];

(*Define a function chessAnimate to animate the chess game*)
chessAnimate[chessHistory_, pgnMovesArray_]:=
Animate[
	Speak[ToString[pgnMovesArray[[i-1]]]]; (* Text to speech on the current move in the PGN notation *)
	(* Display the chessboard and its matrix representation with a grid *)
	Grid[{
	{ChessPlot[chessHistory[[i]],"WhiteOrientation"->"Down","BoardColorSet"->{White,Darker@Gray},"ShowCoordinates"->True,ImageSize->500],ChessPlot[chessHistory[[i]],"MatrixForm"->True]}
	} ],
	{i,1,Length[chessHistory],1}, (* Animate from move 1 to the end of the game *)
	AnimationRate->.5, (* Set animation rate to one move every 2 seconds *)
	 AnimationRunning->False, (* Animation is initially not running, and has to be started by the user *)
	SaveDefinitions->False  
]; 

(* Define a function to extract the opening name from the string in the dataset *)
extractOpeningName[s_String]:=Module[{parts},parts=StringSplit[s,{"|",":"}];
StringTrim[First[parts]]]

(* Function to get indices of games with specific opening and victory status *)
getIndicesWithNamesNormalized[openingSelectedValue_,victorySelectedValue_]:=Module[{indicesWithNames,strings},
	(*Filter dataset to find games with specified parameters*)indicesWithNames=Select[data,#["opening_name"]==openingSelectedValue&&#["victory_status"]==victorySelectedValue&][All,"index"];
	(*Create strings for display,combining indices and opening names*)
	strings=ToString[#]<>" ---- "<>data[[#]][openingNameColumn]&/@indicesWithNames;
	(*Convert list of strings to normal form*)
	indicesWithNamesNormalized=Normal[strings];
	indicesWithNamesNormalized
];

(* Extract unique opening names from the dataset *)
openings=DeleteDuplicates[extractOpeningName/@Normal[data[All,"opening_name"]],SameQ];

(*Extract unique victory statuses from the dataset*)
winTypes=Normal[DeleteDuplicates[data[All,"victory_status"],SameQ]];

(*Define columns for opening name and victory status*)
openingNameColumn="opening_name";
victoryStatusColumn="victory_status";

(*Set default values for opening and victory selection*)
openingSelectedValue="Slav Defense";
victorySelectedValue="resign";

(*Get indices of games with specified opening and victory status*)
indicesWithNames=Select[data,#["opening_name"]==openingSelectedValue&&#["victory_status"]==victorySelectedValue&][All,"index"];

(* Create strings representing games with their opening names *)
strings=ToString[#]<>" ---- "<>data[[#]][openingNameColumn]&/@indicesWithNames;
indicesWithNamesNormalized=Normal[strings];

(*Call the function to retrieve indices of games based on default opening and victory*)
getIndicesWithNamesNormalized[openingSelectedValue,victorySelectedValue];

gamemode = False;
Dynamic[gamemode];
answerResult = "";
Dynamic[answerResult];
reveal = False;
Dynamic[reveal];


(* We initialize a dynamic module for the interface rapresentation. *)
plotGui[]:= DynamicModule[{}, gui=Panel[
	Column[{
		(*Title of the application*)
		Row[{
		Text[Style["Chess Player",Bold,40,Red,FontFamily->"Brush Script MT"]]
		}],
		Dynamic[If[gamemode == False,
		(*Row for selecting opening and victory status*)
		Column[{Row[{
			Text[Style["Opening: ",Bold,20,Green,FontFamily->"Helvetica Neue"]],PopupMenu[Dynamic[openingSelectedValue],openings],Spacer[100],Text[Style["Type of win: ",Bold,20,Green,FontFamily->"Helvetica Neue"]],PopupMenu[Dynamic[victorySelectedValue],winTypes],Spacer[100],Button["Filter",getIndicesWithNamesNormalized[openingSelectedValue,victorySelectedValue]]
			}],
			Row[{InputField[Dynamic[indexOfSelectedGame],Number,FieldHint->"Type the game index here"],Button["Play",{chessHistory, pgnMovesArray,pgnmoves, name, selectedId, typeofwin}=selectedGame[data,ToExpression@indexOfSelectedGame]]}]}, Alignment->Center], ""
		]],
		(*Row for displaying chess animation and game selection*)
		Row[{
		Column[{
		Dynamic[chessAnimate[chessHistory,pgnMovesArray]] (*Display chess game animation*)
		}],
		Spacer[1],
		Dynamic[If[gamemode == False,
		(* Grid containing filtered games *)
		Grid[
		(* Every row contains the game name (id and opening) and a play button to set it as current game *)
		MapThread[{#1,Button["\[FilledRightTriangle]", {{chessHistory, pgnMovesArray,pgnmoves, name, selectedId, typeofwin}=selectedGame[data, ToExpression@First[StringSplit[#1, " "]]](*, Print[ToExpression@First[StringSplit[#1, " "]]]*)} ]}&,{indicesWithNamesNormalized}],Frame->All], ""]],
		(*Button for random game selection*)
		Dynamic[If[gamemode == False, Button["RANDOM",{chessHistory, pgnMovesArray,pgnmoves, name, selectedId, typeofwin}=randomGame[data,dimensioneData]], ""]] 
		}],
		(* Row for displaying current game's moves in PGN format *)
		Dynamic[If[gamemode == False || reveal == True,
		Row[{
		Dynamic[Text[Style[selectedId, Bold, 15, Darker@Gray,FontFamily->"Helvetica Neue"]]],Spacer[3],Dynamic[Text[Style[name, Bold, 15, Darker@Gray,FontFamily->"Helvetica Neue"]]], Spacer[3],Dynamic[Text[Style[typeofwin, Bold, 15, Darker@Gray,FontFamily->"Helvetica Neue"]]]
		}], Text[Style["?", Bold, 15, Darker@Gray,FontFamily->"Helvetica Neue"]]]],
		Row[{
		Dynamic[Text[Style[pgnmoves, Bold, 10, Gray,FontFamily->"Helvetica Neue"]]]
		}],
		Dynamic[If[gamemode == True,
			Column[{
				Text[Style["What's the opening?", Bold, 15, Darker@Gray,FontFamily->"Helvetica Neue"]],
				InputField[Dynamic[answer], String, FieldHint-> "Type your answer here"], Button["Submit", If[answer == name, answerResult = "Correct answer!", answerResult = "Wrong answer!"]],
				Text[Style[Dynamic[answerResult], Bold, 15, Darker@Gray,FontFamily->"Helvetica Neue"]],
				Button["Next question \[RightArrow]",{{chessHistory, pgnMovesArray,pgnmoves, name, selectedId, typeofwin}=randomGame[data,dimensioneData], reveal = False, answerResult = ""}],
				Button["Reveal the opening", reveal = !reveal]
			}, Alignment->Center], ""
		]],
		Button[Dynamic["Gamemode: " <> ToString[gamemode]], {{chessHistory, pgnMovesArray,pgnmoves, name, selectedId, typeofwin}=randomGame[data,dimensioneData], gamemode = !gamemode, reveal = False, answerResult = ""}]
		},Alignment->Center (*Align the GUI components to the center*)
	]
	]
]



End[]
EndPackage[]
