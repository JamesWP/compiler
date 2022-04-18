

int main(int argc, const char* argv[]) {
	int a = 0;


	a++;

	if (a != 1) {return 1;}

	a--;

	if (a != 0) {return 2;}

	
	if (++a != 1) {return 3;}
	if (a++ != 1) {return 4;}
	if (a != 2) {return 5;}

	return 0;
}
