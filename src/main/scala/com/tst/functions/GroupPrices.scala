package com.tst.functions

import com.tst.models.CabinPrice
import com.tst.models.Rate

case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
)

def getBestGroupPrices(
    rates: Seq[Rate],
    prices: Seq[CabinPrice]
): Seq[BestGroupPrice] = {
  val rateGroupMap = rates.map(r => (r.rateCode, r.rateGroup)).toMap
  prices
    .groupBy(f => (f.cabinCode, rateGroupMap(f.rateCode)))
    .values
    .map(f => f.minBy(_.price))
    .map(f =>
      BestGroupPrice(f.cabinCode, f.rateCode, f.price, rateGroupMap(f.rateCode))
    )
    .toSeq
    .sortBy(f => (f.cabinCode, f.rateGroup))

}
