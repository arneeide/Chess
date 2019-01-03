# Chess
A chessboard user interface created in Mathematica

## Installation
Load the Chess` package embedded in the file "Chess.wl" into a Mathematica notebook.

 Needs["Chess`"]

## Usage
So far, no manual has been made, as the work is ongoing. A brief introduction to the capability of the package is however provided in the attached notebook "Chess_package.nb".

## Embedded features
Standard rules of piece moving are installed, including castling, en passant, and the conversion of pawns to officers (standard choice is queen).
Interactive use of the chessboard is included, also a preliminary PGN reading feature (see notebook for details). The chessboard may be displayed in different ways by changing default option values.

## Missing parts
A number of issues are still to be solved:

- The king is not allowed to move into an empty cell covered by pieces of opposite colour. However, the king are able to take pieces even when these are defended.
- The game is not ended when the king is taken.
- There is no evaluation of the game.

## Help is requested
Any contributions in improving existing code and adding features are appreciated. The package is free to be used as long as no commercial interests are involved.

You are welcome to contact me by email: arne.eide@gmail.com
