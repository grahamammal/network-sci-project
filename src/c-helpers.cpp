#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
IntegerMatrix edges_for_session(IntegerMatrix vote_code_mat) {
  
  IntegerMatrix adj_mat(vote_code_mat.nrow(), vote_code_mat.nrow());
  
  for(int i = 0; i < vote_code_mat.ncol(); i++){
    IntegerVector current_vote = vote_code_mat.column(i);
    
    for(int i = 0; i < current_vote.length(); i++) {
      for(int j = 0; j < current_vote.length(); j++) {
        if(current_vote[i] == current_vote[j] && current_vote[i] != -1 && current_vote[j] != -1) {
          adj_mat(i, j) = adj_mat(i, j) + 1;
        }
      }
    }
  }

  return(adj_mat);
}