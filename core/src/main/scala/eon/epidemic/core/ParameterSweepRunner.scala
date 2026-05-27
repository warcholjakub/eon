package eon.epidemic.core

object ParameterSweepRunner:
  def run(
      config: SimulationConfig,
      minProbability: Double,
      maxProbability: Double,
      probabilityStep: Double,
      runsPerPair: Int,
      persistProgress: ParameterSweepResult => Either[String, Unit] = _ => Right(())
  ): Either[String, ParameterSweepResult] =
    probabilityValues(minProbability, maxProbability, probabilityStep).flatMap: probabilities =>
      val pairs =
        for
          infectionProbability <- probabilities
          recoveryProbability <- probabilities
        yield (infectionProbability, recoveryProbability)

      pairs.foldLeft[Either[String, Vector[ParameterSweepRow]]](Right(Vector.empty)):
        case (rowsEither, (infectionProbability, recoveryProbability)) =>
          rowsEither.flatMap: rows =>
            val pairConfig =
              config.copy(
                infectionProbability = infectionProbability,
                recoveryProbability = recoveryProbability
              )
            BatchRunner.aggregate(pairConfig, runsPerPair).flatMap: aggregate =>
              val updatedRows = rows :+ ParameterSweepRow(infectionProbability, recoveryProbability, aggregate)
              val progress = ParameterSweepResult(runsPerPair, updatedRows)
              persistProgress(progress).map(_ => updatedRows)
      .map(rows => ParameterSweepResult(runsPerPair, rows))

  private def probabilityValues(
      minimum: Double,
      maximum: Double,
      step: Double
  ): Either[String, Vector[Double]] =
    if minimum < 0.0 || maximum > 1.0 || minimum > maximum then
      Left("sweep probability range must be within [0.0, 1.0] with minimum <= maximum")
    else if step <= 0.0 then
      Left("sweep probability step must be > 0.0")
    else
      val min = BigDecimal.decimal(minimum)
      val max = BigDecimal.decimal(maximum)
      val increment = BigDecimal.decimal(step)
      val values =
        Iterator
          .iterate(min)(_ + increment)
          .takeWhile(_ <= max)
          .map(_.toDouble)
          .toVector

      Right(values)
