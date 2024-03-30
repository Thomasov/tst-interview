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

  var combinableWith = allPromotions
    .filter(f =>
      f.code != promotion.code && !promotion.notCombinableWith.contains(f.code)
    )

  // var combos: Seq[PromotionCombo] =
  //   Seq(PromotionCombo(combinableWith.map(f => f.code)))

  // go over all combos the promotion codes in the combo
  // if the promotion code is not combinable with all of the other items,
  // - start going over the other combos and check if it is combinable with the code in them
  // - if it isnt combinable with any other combos push it to a new combo and add the combo to the seq

  var combos = Seq.empty[PromotionCombo]

  combinableWith.foreach(currentPromo => {
    if (combos.length == 0) {
      combos = Seq(PromotionCombo(Seq(currentPromo.code, promotionCode)))
    } else {
      var notCombinable = true
      combos.foreach(combo => {
        if (
          combo.promotionCodes
            .forall(f => areCombinable(currentPromo.code, f, allPromotions))
        ) {
          val newCombo =
            PromotionCombo(combo.promotionCodes :+ currentPromo.code)
          combos = combos.updated(combos.indexOf(combo), newCombo)
          notCombinable = false;
        }
      })
      if (notCombinable) {
        combos = combos :+ PromotionCombo(Seq(currentPromo.code, promotionCode))
      }
    }
  })

  return combos.map(f => PromotionCombo(f.promotionCodes.sorted))
}

def areCombinable(
    promotionCode1: String,
    promotionCode2: String,
    allPromotions: Seq[Promotion]
): Boolean = {
  val promotion1 = allPromotions.find(_.code == promotionCode1) match
    case Some(promotion) => promotion
    case None            => return false

  val promotion2 = allPromotions.find(_.code == promotionCode2) match
    case Some(promotion) => promotion
    case None            => return false

  return !promotion1.notCombinableWith.contains(promotion2.code) &&
    !promotion2.notCombinableWith.contains(promotion1.code)
}
