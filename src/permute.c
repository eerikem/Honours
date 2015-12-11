#include "erl_nif.h"
#include <stdio.h>
#include <string.h>

#define MAX_GENERATED_MOVES 60

extern char * permute(char* board);
extern char * permute3(char* board);
extern char * heuristic(char* board);
extern char * heuristic2(char* board);
extern char * connectEval(char* board);
extern char * endState(char* board);
static ERL_NIF_TERM atomX;
static ERL_NIF_TERM atomO;
static ERL_NIF_TERM atomZ;
static ERL_NIF_TERM erlangBoard(ErlNifEnv*, int**);
static ERL_NIF_TERM erlangBoard2(ErlNifEnv*, int**);
void translateBoard(ErlNifEnv*, ERL_NIF_TERM, int[][8]);
void translateConnect(ErlNifEnv*, ERL_NIF_TERM, int[][8]);
ERL_NIF_TERM generateMoves(ErlNifEnv*, int[][8]);
ERL_NIF_TERM generateConnectMoves(ErlNifEnv*, int[][8]);
int ** boardDup(int[][8]);
int ** connectDup(int[][8]);
int move(ErlNifEnv*, int[][8], int***);
int move2(ErlNifEnv*, int[][8], int***, int[]);
int moveConnect(ErlNifEnv*, int[][8], int, int***);


// Copy and return the board on the heap
int ** boardDup(int src[][8]) {
	int **array = (int **) malloc(8 * sizeof(int*));
	int i, x, y;
	for (i = 0; i < 8; i++) {
		array[i] = (int *) malloc(8 * sizeof(int));
	}
	for (x = 0; x < 8; x++) {
		for (y = 0; y < 8; y++) {
			array[x][y] = src[x][y];
		}
	}
	return array;
}

int ** connectDup(int src[][8]) {
	int **array = (int **) malloc(7 * sizeof(int*));
	int i, x, y;
	for (i = 0; i < 7; i++) {
		array[i] = (int *) malloc(8 * sizeof(int));
	}
	for (x = 0; x < 7; x++) {
		for (y = 0; y < 8; y++) {
			array[x][y] = src[x][y];
		}
	}
	return array;
}

void dealloc2(int **array) {
	int x;
	for (x = 0; x < 7; x++) {
		free(array[x]);
	}
	free(array);
}

void dealloc(int **array) {
	int x;
	for (x = 0; x < 8; x++) {
		free(array[x]);
	}
	free(array);
}

// Heuristic that favors the overall
// layout of peaces grows closer to
// the center of the board.
int calcHeuristic(int**board) {
	int x, y;
	int d = 0;
	for (y = 1; y <= 7; y++) {
		for (x = 1; x <= 7; x++) {
			if (board[y][x] == 1) {
				d += abs(y - 4) + abs(x - 4);
			}
		}
	}
	return d;
}

// Generate heuristic values during the permutation of the new moves.
static ERL_NIF_TERM heuristic_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM nif_board = argv[0];
	if (!enif_is_list(env, nif_board))
		return enif_make_badarg(env);

	int board[8][8] = { 0 };
	translateBoard(env, nif_board, board);

	int***moves = (int***) malloc(MAX_GENERATED_MOVES * sizeof(int**));
	int cnt = move(env, board, moves);
	int length;
	int i;
	ERL_NIF_TERM results[MAX_GENERATED_MOVES];
	ERL_NIF_TERM h, newBoard;
	for (i = 0; i < cnt; i++) {
		newBoard = erlangBoard(env, moves[i]);
		h = enif_make_int(env, calcHeuristic(moves[i]));
		results[i] = enif_make_tuple(env, 2, h, newBoard);
		dealloc(moves[i]);
	}
	free(moves);

	return enif_make_list_from_array(env, results, cnt);

}

// Generate second heuristic values during the permutation of the new moves.
static ERL_NIF_TERM heuristic2_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM nif_board = argv[0];
	if (!enif_is_list(env, nif_board))
		return enif_make_badarg(env);

	int board[8][8] = { 0 };
	translateBoard(env, nif_board, board);

	int***moves = (int***) malloc(MAX_GENERATED_MOVES * sizeof(int**));
	int h_list[MAX_GENERATED_MOVES];
	int cnt = move2(env, board, moves, h_list);
	int length;
	int i;
	int h1, h2;
	ERL_NIF_TERM results[MAX_GENERATED_MOVES];
	ERL_NIF_TERM h, newBoard;
	for (i = 0; i < cnt; i++) {
		newBoard = erlangBoard(env, moves[i]);
		h1 = calcHeuristic(moves[i]);
		h2 = h_list[i];
		results[i] = enif_make_tuple(env, 2,
				enif_make_int(env, (h1 * 2 + h2) / 2), newBoard);
		dealloc(moves[i]);
	}
	free(moves);

	return enif_make_list_from_array(env, results, cnt);

}

// Translate the board from Erlang to int[][]
void translateBoard(ErlNifEnv* env, ERL_NIF_TERM nifBoard, int (*board)[8]) {
	atomX = enif_make_atom(env, "x");
	atomO = enif_make_atom(env, "o");
	ERL_NIF_TERM rows, head, tail, position;
	int empty = -1;
	int taken = 1;
	int row = 1;
	int column = 1;
	int rowSize = 0;
	while (enif_get_list_cell(env, nifBoard, &head, &rows)) {
		enif_get_list_length(env, head, &rowSize);
		column = (7 - rowSize) / 2 + 1;
		while (enif_get_list_cell(env, head, &position, &tail)) {
			if (position == atomX) {
				board[row][column] = taken;
			} else {
				board[row][column] = empty;
			}
			column++;
			head = tail;
		}
		nifBoard = rows;
		row++;
	}
}

void translateConnect(ErlNifEnv* env, ERL_NIF_TERM nifBoard, int (*board)[8]) {
	atomX = enif_make_atom(env, "x");
	atomO = enif_make_atom(env, "o");
	atomZ = enif_make_atom(env, "z");
	ERL_NIF_TERM rows, head, tail, position;
	int empty = -1;
	int ex = 1;
	int zed = 2;
	int row = 1;
	int column = 1;
	int rowSize = 0;
	while (enif_get_list_cell(env, nifBoard, &head, &rows)) {
		row = 1;
		//enif_get_list_length(env, head, &rowSize);
		while (enif_get_list_cell(env, head, &position, &tail)) {
			if (position == atomX) {
				board[row][column] = ex;
				//fprintf(stderr,"Found an x at %d, %d\n",row, column);
			} else if (position == atomZ) {
				board[row][column] = zed;
			} else {
				board[row][column] = empty;
			}
			row++;
			head = tail;
		}
		nifBoard = rows;
		column++;
	}
}

// A simple permutation of all possible
// moves for a given state.
static ERL_NIF_TERM permit_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM nif_board = argv[0];
	if (!enif_is_list(env, nif_board))
		return enif_make_badarg(env);

	int board[8][8] = { 0 };
	translateBoard(env, nif_board, board);
	return generateMoves(env, board);
}

static ERL_NIF_TERM permit3_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM nif_board = argv[0];
	if (!enif_is_list(env, nif_board))
		return enif_make_badarg(env);

	int board[7][8] = { 0 };
	translateConnect(env, nif_board, board);
	//int **b = connectDup(board);
	//return erlangBoard2(env, b);
	return generateConnectMoves(env, board);
}

// Given an integer board generate the new
// moves and translate them back to Erlang
ERL_NIF_TERM generateMoves(ErlNifEnv* env, int board[][8]) {
	int***moves = (int***) malloc(MAX_GENERATED_MOVES * sizeof(int**));
	int cnt = move(env, board, moves);
	int length;
	int i;
	ERL_NIF_TERM results[MAX_GENERATED_MOVES];
	for (i = 0; i < cnt; i++) {
		results[i] = erlangBoard(env, moves[i]);
		dealloc(moves[i]);
	}
	free(moves);
	return enif_make_list_from_array(env, results, cnt);
}

int whosMove(int board[][8]) {
	int x, y;
	int cntX = 0;
	int cntZ = 0;
	for (x = 1; x <= 6; x++) {
		for (y = 1; y <= 7; y++) {
			if (board[x][y] == 1)
				cntX++;
			if (board[x][y] == 2)
				cntZ++;
		}
	}
	if (cntX > cntZ)
		return 2;
	return 1;
}

ERL_NIF_TERM generateConnectMoves(ErlNifEnv* env, int board[][8]) {
	int***moves = (int***) malloc(MAX_GENERATED_MOVES * sizeof(int**));
	int cnt = moveConnect(env, board, whosMove(board), moves);
	int length;
	int i;
	ERL_NIF_TERM results[MAX_GENERATED_MOVES];
	for (i = 0; i < cnt; i++) {
		results[i] = erlangBoard2(env, moves[i]);
		dealloc2(moves[i]);
	}
	free(moves);
	return enif_make_list_from_array(env, results, cnt);
}
// The actual permutation of new moves
int moveConnect(ErlNifEnv* env, int board[][8], int player, int ***a) {
	int p1 = 1;
	int p2 = 2;
	int empty = -1;
	int **b = connectDup(board);
	int cnt = 0;
	int x, y;
	for (y = 1; y <= 7; y++) {
		for (x = 6; x >= 1; x--) {
			if (board[x][y] == -1) {
				b[x][y] = player;
				a[cnt] = b;
				b = connectDup(board);
				cnt++;
				break;
			}

			if (cnt > MAX_GENERATED_MOVES - 4) {
				fprintf(stderr,
						"***WARNING***  Reached MAX_GENERATED_MOVES: %d\n",
						MAX_GENERATED_MOVES);
				return cnt;
			}
		}
	}
	return cnt;
}

// The actual permutation of new moves
int move(ErlNifEnv* env, int board[][8], int ***a) {
	int taken = 1;
	int empty = -1;
	int **b = boardDup(board);
	int cnt = 0;
	int x, y;
	for (y = 1; y <= 7; y++) {
		for (x = 1; x <= 7; x++) {
			if (x == 3 || (y >= 3 && y <= 5)) {
				//xxo -> oox
				if (x <= 5 && board[y][x] == taken && board[y][x + 1] == taken
						&& board[y][x + 2] == empty) {
					b[y][x] = empty;
					b[y][x + 1] = empty;
					b[y][x + 2] = taken;
					a[cnt] = b;
					b = boardDup(board);
					cnt++;
				}
				//oxx -> xoo
				if (x <= 5 && board[y][x] == empty && board[y][x + 1] == taken
						&& board[y][x + 2] == taken) {
					b[y][x] = taken;
					b[y][x + 1] = empty;
					b[y][x + 2] = empty;
					a[cnt] = b;
					b = boardDup(board);
					cnt++;
				}

				if (x <= 5 && board[x][y] == empty && board[x + 1][y] == taken
						&& board[x + 2][y] == taken) {
					b[x][y] = taken;
					b[x + 1][y] = empty;
					b[x + 2][y] = empty;
					a[cnt] = b;
					b = boardDup(board);
					cnt++;
				}

				if (x <= 5 && board[x][y] == taken && board[x + 1][y] == taken
						&& board[x + 2][y] == empty) {
					b[x][y] = empty;
					b[x + 1][y] = empty;
					b[x + 2][y] = taken;
					a[cnt] = b;
					b = boardDup(board);
					cnt++;
				}

				if (cnt > MAX_GENERATED_MOVES - 4) {
					fprintf(stderr,
							"***WARNING***  Reached MAX_GENERATED_MOVES: %d\n",
							MAX_GENERATED_MOVES);
					return cnt;
				}
			}
		}
	}
	return cnt;
}

int inBounds(int x, int y) {
	if ((x >= 3 && x <= 5) || (y >= 3 && y <= 5)) {
		if ((x >= 1 && x <= 7) && (y >= 1 && y <= 7)) {
			return 1;
		}
	}
	return 0;
}

int calcH2(int a[][8], int**b) {
	int x, y;
	int r = 0;
	for (y = 1; y <= 7; y++) {
		for (x = 1; x <= 7; x++) {
			if (inBounds(x, y)) {
				if (a[y][x] != b[y][x] && b[y][x] == 1) {
					if (!inBounds(x, y - 1) || b[y - 1][x] == -1)
						r++;
					if (!inBounds(x, y + 1) || b[y + 1][x] == -1)
						r++;
					if (!inBounds(x - 1, y) || b[y][x - 1] == -1)
						r++;
					if (!inBounds(x + 1, y) || b[y][x + 1] == -1)
						r++;
					return r;
				}
			}
		}
	}
}

// The actual permutation of new moves
int move2(ErlNifEnv* env, int board[][8], int ***a, int h[]) {
	int taken = 1;
	int empty = -1;
	int **b = boardDup(board);
	int cnt = 0;
	int x, y;
	for (y = 1; y <= 7; y++) {
		for (x = 1; x <= 7; x++) {
			if (x == 3 || (y >= 3 && y <= 5)) {
				//xxo -> oox
				if (x <= 5 && board[y][x] == taken && board[y][x + 1] == taken
						&& board[y][x + 2] == empty) {
					b[y][x] = empty;
					b[y][x + 1] = empty;
					b[y][x + 2] = taken;
					a[cnt] = b;
					h[cnt] = calcH2(board, b);
					b = boardDup(board);
					cnt++;
				}
				//oxx -> xoo
				if (x <= 5 && board[y][x] == empty && board[y][x + 1] == taken
						&& board[y][x + 2] == taken) {
					b[y][x] = taken;
					b[y][x + 1] = empty;
					b[y][x + 2] = empty;
					a[cnt] = b;
					h[cnt] = calcH2(board, b);
					b = boardDup(board);
					cnt++;
				}

				if (x <= 5 && board[x][y] == empty && board[x + 1][y] == taken
						&& board[x + 2][y] == taken) {
					b[x][y] = taken;
					b[x + 1][y] = empty;
					b[x + 2][y] = empty;
					a[cnt] = b;
					h[cnt] = calcH2(board, b);
					b = boardDup(board);
					cnt++;
				}

				if (x <= 5 && board[x][y] == taken && board[x + 1][y] == taken
						&& board[x + 2][y] == empty) {
					b[x][y] = empty;
					b[x + 1][y] = empty;
					b[x + 2][y] = taken;
					a[cnt] = b;
					h[cnt] = calcH2(board, b);
					b = boardDup(board);
					cnt++;
				}

				if (cnt > MAX_GENERATED_MOVES - 4) {
					fprintf(stderr,
							"***WARNING***  Reached MAX_GENERATED_MOVES: %d\n",
							MAX_GENERATED_MOVES);
					return cnt;
				}
			}
		}
	}
	return cnt;
}

static ERL_NIF_TERM erlangBoard(ErlNifEnv* env, int **board) {
	int x, y, i;
	ERL_NIF_TERM rows[7];
	ERL_NIF_TERM row[8];
	for (y = 1; y <= 7; y++) {
		if (y < 3 || y > 5) {
			for (x = 3; x <= 5; x++) {
				if (board[y][x] == 1)
					row[x] = atomX;
				else
					row[x] = atomO;
			}
			rows[y - 1] = enif_make_list_from_array(env, &row[3], 3);
		} else {
			for (x = 1; x <= 7; x++) {
				if (board[y][x] == 1)
					row[x] = atomX;
				else
					row[x] = atomO;
			}
			rows[y - 1] = enif_make_list_from_array(env, &row[1], 7);
		}
	}

	ERL_NIF_TERM result = enif_make_list_from_array(env, rows, 7);
	return result;
}

static ERL_NIF_TERM erlangBoard2(ErlNifEnv* env, int **board) {
	int x, y, i;
	ERL_NIF_TERM rows[7];
	ERL_NIF_TERM column[7];
	for (y = 1; y <= 7; y++) {
		for (x = 1; x <= 6; x++) {
			if (board[x][y] == 1) {
				column[x] = atomX;
			} else if (board[x][y] == 2) {
				column[x] = atomZ;
			} else {
				column[x] = atomO;
			}
		}
		rows[y - 1] = enif_make_list_from_array(env, &column[1], 6);
	}
	ERL_NIF_TERM result = enif_make_list_from_array(env, rows, 7);
	return result;
}

static int hasWinner(int board[][8]) {
	int x, y;
	int xCnt = 0;
	int zCnt = 0;
	for (y = 1; y <= 7; y++) {
		for (x = 1; x <= 6; x++) {
			if (board[x][y] == 1) {
				xCnt++;
				zCnt = 0;
			} else if (board[x][y] == 2) {
				zCnt++;
				xCnt = 0;
			} else {
				xCnt = 0;
				zCnt = 0;
			}
			if (xCnt == 4)
				return 1;
			if (zCnt == 4)
				return 2;
		}
		xCnt = 0;
		zCnt = 0;
	}
	for (y = 1; y <= 6; y++) {
		for (x = 1; x <= 7; x++) {
			if (board[y][x] == 1) {
				xCnt++;
				zCnt = 0;
			} else if (board[y][x] == 2) {
				zCnt++;
				xCnt = 0;
			} else {
				xCnt = 0;
				zCnt = 0;
			}
			if (xCnt == 4)
				return 1;
			if (zCnt == 4)
				return 2;
		}
		xCnt = 0;
		zCnt = 0;
	}

	for (y = 1; y <= 6; y++) {
		for (x = 1; x <= 7; x++) {
			int a = y;
			int b = x;
			while (a <= 6 && b <= 7) {
				if (board[a][b] == 1) {
					xCnt++;
					zCnt = 0;
				} else if (board[a][b] == 2) {
					zCnt++;
					xCnt = 0;
				} else {
					xCnt = 0;
					zCnt = 0;
				}
				if (xCnt == 4)
					return 1;
				if (zCnt == 4)
					return 2;
				a++;
				b++;
			}
			xCnt = 0;
			zCnt = 0;
		}
	}

	for (y = 1; y <= 6; y++) {
		for (x = 1; x <= 7; x++) {
			int a = y;
			int b = x;
			while (a >= 1 && b <= 7) {
				if (board[a][b] == 1) {
					xCnt++;
					zCnt = 0;
				} else if (board[a][b] == 2) {
					zCnt++;
					xCnt = 0;
				} else {
					xCnt = 0;
					zCnt = 0;
				}
				if (xCnt == 4)
					return 1;
				if (zCnt == 4)
					return 2;
				a--;
				b++;
			}
			xCnt = 0;
			zCnt = 0;
		}
	}
	return 0;
}

// Determines if connect four
// board has reached end state.
static ERL_NIF_TERM endState_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM nif_board = argv[0];
	if (!enif_is_list(env, nif_board))
		return enif_make_badarg(env);

	int board[7][8] = { 0 };
	translateConnect(env, nif_board, board);
	int x, y;
	int xCnt = 0;
	int zCnt = 0;

	if (hasWinner(board))
		return enif_make_atom(env, "true");

	for (y = 1; y <= 7; y++) {
		for (x = 1; x <= 6; x++) {
			if (board[x][y] == -1) {
				return enif_make_atom(env, "false");
			}
		}
	}
	return enif_make_atom(env, "true");
}

//Generate minimax heuristic values
static ERL_NIF_TERM connectEval_nif(ErlNifEnv* env, int argc,
		const ERL_NIF_TERM argv[]) {
	ERL_NIF_TERM nif_board = argv[0];
	if (!enif_is_list(env, nif_board))
		return enif_make_badarg(env);

	ERL_NIF_TERM win = enif_make_int(env, 1000);
	ERL_NIF_TERM loose = enif_make_int(env, -1000);
	ERL_NIF_TERM draw = enif_make_int(env, 0);

	int board[7][8] = { 0 };
	translateConnect(env, nif_board, board);

	if (hasWinner(board) == 1)
		return win;
	if (hasWinner(board) == 2)
		return loose;

	int player = whosMove(board);
	int sum = 0,x,y;
	for (x = 1; x <= 6; x++) {
		for (y = 1; y <= 7; y++) {
			if (board[x][y] == 1) {
				sum += abs(y - 4) + abs(x - 3);
			}
		}
	}
	//if(player == 2)sum = -sum;
	return enif_make_int(env, 1000-sum);
}

static ErlNifFunc nif_funcs[] = { { "permute", 1, permit_nif }, { "heuristic",
		1, heuristic_nif }, { "heuristic2", 1, heuristic2_nif }, { "permute3",
		1, permit3_nif }, { "connectEval", 1, connectEval_nif }, { "endState",
		1, endState_nif } };

ERL_NIF_INIT(permute, nif_funcs, NULL, NULL, NULL, NULL)
