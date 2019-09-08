# Analysis of a Retirement Product

This repository contains the code and report for the project from
Acma 320 at SFU in Spring 2014. 

## Product

ABC Life Insurance Company has a new annuity product which provides a regular
income starting at age 65 funded by regular premiums until age 65. The
retirement income is most commonly paid monthly, but policyholders can choose a
different payment frequency. Policyholders can also choose the premium
frequency. If the policyholder dies before 65, the product doesn't pay anything.
At purchase, the policyholder also chooses a guarantee period of 0 to 20 years
in length. 

## Assumptions

* Mortality
	* SSSM with A = 0.00022, B = 0.000025 and c = 1.1 and limiting age 120.
	* UDD for fractional ages
* Policyholders
	* 10,000 policyholders
	* 20% age 30-40, 30% age 40-50, 50% age 50-60
* Benefit
	* 50,000 per policy on average
* Interest Rate
	* 5% earned on investments

## Task 

* Calculate nominal annual benefit premiums
* Calculate expected value and standard deviation of loss random variable
* Graph probability and distribution functions of loss random variable
* Find expected value and standard deviation of prospective loss random variable
* Analyze interest rate sensitivity and apply concept of duration to liabilities
  and assets
* Recommend annual gross premiums
* Find expected value and standard deviation of propsective loss random variable
  using recommended gross premiums

## Files

* R/ 			contains code using in analysis
* Report/		contains Latex report (report.pdf)
* Excel/		contains excel spreadsheet for analysis
