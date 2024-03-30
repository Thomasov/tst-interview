package com.tst.functions

import com.tst.models.Promotion

case class PromotionCombo(promotionCodes: Seq[String])

def allCombinablePromotions(
    allPromotions: Seq[Promotion]
): Seq[PromotionCombo] = {
  Seq.empty
}
