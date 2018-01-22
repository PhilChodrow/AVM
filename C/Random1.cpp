/*
 * Random1.cpp
 *
 *  Created on: Apr 23, 2011
 *      Author: Administrator
 */

#include "Random1.h"
#include <cstdlib>
#include <ctime>

Random1::Random1()
{
	srand((unsigned)time(NULL));
//	srand(5);
}

Random1::~Random1() {
	// TODO Auto-generated destructor stub
}

double Random1::real()
{
	return (double)rand()/(double)RAND_MAX;
}

long int Random1::integer(long int n)
{
	long int i1;
	while ((i1=(long int)(real()*n))==n)
		;
	return i1;
}
