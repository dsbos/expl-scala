package com.us.dsb.explore.movethis


object ScheduleStep {

  /**
    * Creates dummy base scheduling step.
    */
  def apply[TIME](initialDroneAvailabilityTime: TIME): ScheduleStep[TIME] = {
    ScheduleStep(initialDroneAvailabilityTime,
                 "Dummy starting scheduling step",
                 initialDroneAvailabilityTime,  // ??: -> None
                 None,
                 0, 0, 0,
                 initialDroneAvailabilityTime)
  }
}

/**
  * TODO: Update per changes to turn into ScheduleStop:
  * Minimum state needed to score subsequent orders.  (Does not include
  * scheduled order(s).)
  *
  * @param  promCount  number of orders expecting promoter-level satisfaction
  * @param  neutCount  number of orders expecting neutral satisfaction
  * @param  detrCount  number of orders expecting detractor-level satisfaction
  * @param  nextAvailableTime  time drone is available to start next delivery
  * @tparam  TIME
  */
case class ScheduleStep[TIME](startingAvailableTime: TIME,
                              orderId: String,
                              departureTime: TIME,  // TODO:  Add Option[] as for deliveryTime
                              deliveryTime: Option[TIME],
                              promCount: Int,
                              neutCount: Int,
                              detrCount: Int,
                              nextAvailableTime: TIME) {

  def withStartingAvailableTime(newValue: TIME): ScheduleStep[TIME] = {
    copy(startingAvailableTime = newValue)
  }
  def withOrderId(newValue: String): ScheduleStep[TIME] = {
    copy(orderId = newValue)
  }
  def withDepartureTime(newValue: TIME): ScheduleStep[TIME] = {
    copy(departureTime = newValue)
  }
  def withDeliveryTime(newValue: Option[TIME]): ScheduleStep[TIME] = {
    copy(deliveryTime = newValue)
  }
  def withIncrementedSatCat(satCat: SatisfactionCategory): ScheduleStep[TIME] = {
    satCat match {
      case Promoter  => copy(promCount = promCount + 1)
      case Neutral   => copy(neutCount = neutCount + 1)
      case Detractor => copy(detrCount = detrCount + 1)
    }
  }
  def withNextAvailableTime(newValue: TIME): ScheduleStep[TIME] = {
    copy(nextAvailableTime = newValue)
  }


  private def npsOf(promCount: Int, neutCount: Int, detrCount: Int): Float = {
    val total = promCount + neutCount + detrCount
    val promPct = 100f * promCount / total  // x / 0f yield NaN (not exception)
    val detrPct = 100f * detrCount / total
    val npsPct = promPct - detrPct
    npsPct
  }

  /**
    * Expected net-promoter score.
    */
  val npsPct = npsOf(promCount, neutCount, detrCount)

  /**
    * Calculates the maximum NPS that is possible from this state with a given
    * number of additional orders.  (Intended for traversal pruning.)
    */
  def maxPossibleNps(additional: Int): Float = {
    val maxPossiblePromCount  = promCount + additional
    val minPossibleDetrCount = detrCount
    val maxNps = npsOf(maxPossiblePromCount, neutCount, minPossibleDetrCount)
    maxNps
  }

  /**
    * Calculates the minimum NPS that is possible from this state with a given
    * number of additional orders.  (Intended for traversal pruning.)
    */
  def minPossibleNps(additional: Int): Float = {
    val minPossiblePromCount  = promCount
    val maxPossibleDetrCount  = detrCount + additional
    val minNps = npsOf(minPossiblePromCount, neutCount, maxPossibleDetrCount)
    minNps
  }
}

