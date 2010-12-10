;;;; FiveAM testing for Sassetti
;;;;
;;;; See COPYING for copyright and licensing information.
(in-package #:sassetti)

(def-suite user-funcs :description "Test user Sassetti's build-in user functions" :in sassetti)
(in-suite user-funcs)

(test depreciate

  (is (equal
"2010/10/15 Depreciate Internet access
      Assets:Prepaid:Internet access            -30
      Expenses:Depreciation:Internet access     30
                                                

2010/11/15 Depreciate Internet access
      Assets:Prepaid:Internet access            -30
      Expenses:Depreciation:Internet access     30
                                                

" (string-form (depreciate 2010 10 "Internet access" 60 2))))

  (is (equal 
"2010/10/15 Depreciate 7 month gym membership
      Assets:Prepaid:7 month gym membership            $-33.71
      Expenses:Depreciation:7 month gym membership     $33.71
                                                       

2010/11/15 Depreciate 7 month gym membership
      Assets:Prepaid:7 month gym membership            $-33.71
      Expenses:Depreciation:7 month gym membership     $33.71
                                                       

2010/12/15 Depreciate 7 month gym membership
      Assets:Prepaid:7 month gym membership            $-33.72
      Expenses:Depreciation:7 month gym membership     $33.72
                                                       

2011/01/15 Depreciate 7 month gym membership
      Assets:Prepaid:7 month gym membership            $-33.71
      Expenses:Depreciation:7 month gym membership     $33.71
                                                       

2011/02/15 Depreciate 7 month gym membership
      Assets:Prepaid:7 month gym membership            $-33.72
      Expenses:Depreciation:7 month gym membership     $33.72
                                                       

2011/03/15 Depreciate 7 month gym membership
      Assets:Prepaid:7 month gym membership            $-33.71
      Expenses:Depreciation:7 month gym membership     $33.71
                                                       

2011/04/15 Depreciate 7 month gym membership
      Assets:Prepaid:7 month gym membership            $-33.72
      Expenses:Depreciation:7 month gym membership     $33.72
                                                       

" (string-form (depreciate 2010 10 "7 month gym membership" $236 7))))
  )