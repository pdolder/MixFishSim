#include <Rcpp.h>
using namespace Rcpp;

////////////////////////////////////////////
/// distance calculation function       ////
////////////////////////////////////////////

//' @title distance calculation
//'
//' @description \code{distance_calc} calculates the euclidean distance between
//' two cell references.
//'
//' @param x1 is an integar for the starting x position
//' @param y1 is an integar for the starting y position
//' @param x2 is an integar for the end x position
//' @param y2 is an integar for the end y position
//'
//' @return is a distance between the two cells
//'
//' @examples distance_calc(2, 3, 5, 7) 
//'
//' @export
//  [[Rcpp::export]]

double distance_calc(int x1, int y1, int x2, int y2) {
	int x = x1 - x2;
	int y = y1 - y2;
	double dist = pow(x,2) + pow(y,2);
	dist = sqrt(dist);
	return dist;
}

////////////////////////////////////////////
/// Movement probability function       ////
////////////////////////////////////////////

//' @title movement probability function 
//'
//' @description \code{move_prob} calculates the movement probability between 
//' a cell and all other cells based on the distance and \emph{lambda}.
//'
//' @param start is a Numeric vector of dim 2 for the starting position c(x,y)
//' @param lambda is an integar for the value for the exponential decay in probability 
//' of movement, i.e. \eqn{Pr(B|A) = \exp{-\lambda*dist_{a,b}} / Sum(c=1:c=n)  \exp{-\lambda*dist}}
//' @param hab is a matrix of the habitat suitability
//'
//' @return is a matrix of the movement probabilities from a cell
//'
//' @examples move_prob(c(2, 5), 0.3, matrix(nc = 3, runif(9))) 
//' 
//' @export
//  [[Rcpp::export]]
NumericMatrix move_prob(NumericVector start, double lambda, NumericMatrix hab) {

 // Define the matrix size
 int nrow = hab.nrow(); 
 int ncol = hab.ncol();
 
 // Define the output
 NumericMatrix out(nrow, ncol);
 
 // Calc the move probability for the cell
 for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
 
// calculate the distance between cells
 double d = distance_calc(start(0), start(1), i, j );

 // calculate the movement probability and store in matrix
 out(i,j) = exp( -lambda * d ) * pow( hab( i, j ), 2 );
		     }
	     }

 // divide by the sum
 double sumout = sum(out);
 out = out / sumout;

 // return matrix
 return out;

}

//////////////////////////////////////////////////////////////////////////////
// The movement probability function - this time as a list of all positions //
// To provide probability of movement from any position                     //
//////////////////////////////////////////////////////////////////////////////

//' @title movement probability function as a list 
//'
//' @description \code{move_prob_list} applies \code{\link{move_prob}} from all
//' cells to all other cells and returns as a list.
//'
//' @param lambda is the decay value as in \code{move_prob} 
//' @param hab is a matrix of the habitat suitability for the population
//'
//' @return is a list of the movement probabilities form each cell to all other
//' cells
//'
//' @examples None at the moment
//'
//' @export
//  [[Rcpp::export]]
List move_prob_Lst(double lambda, NumericMatrix hab) {

 // Define the matrix size
 int nrow = hab.nrow(); 
 int ncol = hab.ncol();
 int n = ncol * nrow;
 
 
 // List output and counter
 List outLst(n);
 int counter = 0;

 // for each element of the list, calculate the move prob
  
 for(int a = 0; a < nrow; a++) {
 for (int b = 0; b < ncol; b++) {
 
 // Define the output
 NumericMatrix out(nrow, ncol);
 int counter2 = counter;  // capture counter state
 counter2++; // capture counter + n steps

 // Calc the move probability for the cell
 for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
 
// calculate the distance between cells
 double d = distance_calc(a, b, i, j );

 // calculate the movement probability and store in matrix
 out(i,j) = exp( -lambda * d ) * pow( hab( i, j ), 2 );
		     }
	     }

 // divide by the sum
 double sumout = sum(out);
 out = out / sumout;

 // return matrix to list
 outLst[counter] = out;

 counter = counter2;  // update counter state

}
}

// return list of matrices
return outLst;

}

///////////////////////////////////////////////////////////////////////////////
//// Population movement function - to return a matrix of the new population //
//// locations following the probability function                            //
///////////////////////////////////////////////////////////////////////////////

//' @title population movement function 
//'
//' @description \code{move_population} redistributes the population based on
//' the movement probabilities
//' @param moveProp is a list of the proportion of the population from each
//' cell to reallocated to each of the other cells
//' @param StartPop is a Numeric Matrix of the current populations distribution 
//'
//' @return is a list of the new position for the population from each of the
//' cells. 
//' 
//' NOTE: This is not aggregated and requires calling the R function
//' \emph{Reduce('+', Lst)} to reaggregate. Would be better if done in function
//' but Reduce is currently faster...but much more memory intensive to get out
//' the lists...using the standard c++ accumulate
//' function may work for this but untested
//'
//' @examples None at the moment
//'
//' @export
// [[Rcpp::export]]
List move_population(List moveProp, NumericMatrix StartPop) {

// Define matrix size
 int nrow = StartPop.nrow();
 int ncol = StartPop.ncol();
 int n = ncol * nrow;

// Define the output
 NumericMatrix SumoutPop(nrow,ncol); // Sum of all the pop across the cells
 List outPopLst(n);                  // For storing the list of pops from cell
 int counter = 0;                    // initialise the counter

// For each row and column in the present population 
 for(int a = 0; a < nrow; a++) {
	 for(int b = 0; b < ncol; b++) {

     int counter2 = counter; // counter state for position in list
     counter2++;             // update counter steps
      
     NumericMatrix Props = moveProp[counter];  // update probs
     double CellPop = StartPop(a,b);           // Get cell population
     NumericMatrix outPop(nrow, ncol);         // For storing the pop from cell

 // Calculate the distribution of the population from the cell 
 for (int i = 0; i < nrow; i++) {
	 for (int j = 0; j < ncol; j++) {
		 
 // Allocate cell pop to the new pop matrix
 outPop(i,j) = Props(i,j) * CellPop;
 // std::cout << Props(i,j) << std::endl;

	 }
 }

 // return to list at end of cell loop
 outPopLst[counter] = outPop;
 counter = counter2; // update the counter
	 
	 }
  }

// Sum the list of matrices
 
// for (int i = 0; i < nrow; i++) {
//	 for (int j = 0; j < ncol; j++) 	 {

//	 double sum = 0; // initialise the sum container 
	 
//	 for (int c = 0; c < n; c++) {
//		 NumericMatrix CellPopl = outPopLst[c];
 //                 sum = sum + CellPopl(i,j); // add to sum
//		 }

//	SumoutPop(i,j) = sum; // return sum to index

//	 }
 //}


 // Return the matrix
// return SumoutPop;
 return outPopLst; 

}

