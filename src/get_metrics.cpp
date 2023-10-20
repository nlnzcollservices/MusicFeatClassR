
#include <Rcpp.h>
using namespace Rcpp;

// F1-score
double calculateF1Score(NumericMatrix confusion_matrix) {
  int n = confusion_matrix.nrow();

  NumericVector f1_scores(n);

  for (int i = 0; i < n; i++) {
    double true_positive = confusion_matrix(i, i);
    double false_positive = sum(confusion_matrix(_, i)) - true_positive;
    double false_negative = sum(confusion_matrix(i, _)) - true_positive;

    double precision = true_positive / (true_positive + false_positive);
    double recall = true_positive / (true_positive + false_negative);

    f1_scores[i] = 2 * (precision * recall) / (precision + recall);
  }

  return mean(f1_scores);
}

//  Matthew Correlation Coefficient (MCC)
double calculateMCC(NumericMatrix confusion_matrix) {
  int n = confusion_matrix.nrow();

  NumericVector mcc_values(n);

  for (int i = 0; i < n; i++) {
    double true_positive = confusion_matrix(i, i);
    double false_positive = sum(confusion_matrix(_, i)) - true_positive;
    double false_negative = sum(confusion_matrix(i, _)) - true_positive;
    double true_negative = sum(confusion_matrix) - true_positive - false_positive - false_negative;

    mcc_values[i] = (true_positive * true_negative - false_positive * false_negative) /
      sqrt((true_positive + false_positive) * (true_positive + false_negative) *
        (true_negative + false_positive) * (true_negative + false_negative));
  }

  return mean(mcc_values);
}

// Rcpp export functions
// Input: confusion_matrix
// Output: F1-score and MCC
// [[Rcpp::export]]
NumericVector classificationMetrics(NumericMatrix confusion_matrix) {
  double f1_score = calculateF1Score(confusion_matrix);
  double mcc = calculateMCC(confusion_matrix);

  NumericVector metrics = NumericVector::create(Named("F1_Score") = f1_score,
                                                Named("MCC") = mcc);

  return metrics;
}
