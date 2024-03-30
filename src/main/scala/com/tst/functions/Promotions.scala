package com.tst.functions

import com.tst.models.Promotion

case class PromotionCombo(promotionCodes: Seq[String])

def allCombinablePromotions(
    allPromotions: Seq[Promotion]
): Seq[PromotionCombo] = {
  allPromotions
    .flatMap(promotion => combinablePromotions(promotion.code, allPromotions))
    .distinct
}

def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
): Seq[PromotionCombo] = {

  val promotion = allPromotions.find(_.code == promotionCode) match
    case Some(promotion) => promotion
    case None            => return Seq.empty

  var combinableWith = allPromotions.filter(f => f.code != promotion.code && !promotion.notCombinableWith.contains(f.code))

  var combos = Seq.empty[PromotionCombo]

  combinableWith.foreach(currentPromo => {
    if (combos.length == 0) {
      combos = Seq(PromotionCombo(Seq(currentPromo.code, promotionCode)))
    } else {
      //check if the current promotion can be combined with any of the existing combos
      // if it can, add it to the combo, otherwise create a new combo
      var isNotCombinable = true
      combos.foreach(combo => {
        if (combo.promotionCodes.forall(f => areCombinable(allPromotions, currentPromo.code, f))) {
          val newCombo =PromotionCombo(combo.promotionCodes :+ currentPromo.code)
          combos = combos.updated(combos.indexOf(combo), newCombo)
          isNotCombinable = false;
        }
      })
      if (isNotCombinable) {
        combos = combos :+ PromotionCombo(Seq(currentPromo.code, promotionCode))
      }
    }
  })

  return combos.map(f => PromotionCombo(f.promotionCodes.sorted))
}

def areCombinable(
    allPromotions: Seq[Promotion],
    promotionCode1: String,
    promotionCode2: String
): Boolean = {
  val promotion1 = allPromotions.find(_.code == promotionCode1) match
    case Some(promotion) => promotion
    case None            => return false

  val promotion2 = allPromotions.find(_.code == promotionCode2) match
    case Some(promotion) => promotion
    case None            => return false

  return !promotion1.notCombinableWith.contains(promotion2.code) && !promotion2.notCombinableWith.contains(promotion1.code)
}
