import daisy.lang._
import Vector._

object MountainCarNN {

  def nn1(x: Vector): Vector = {
    require(lowerBounds(x, List(-1.2, -0.07)) &&
      upperBounds(x, List(0.6, 0.07)))

    val weights1 = Matrix(List(
      List(-3.48562848e-01, -1.65240574e+00),
      List(-5.85713185e-01, 4.43186055e-01),
      List( 2.44280583e-01, 1.63036100e+00),
      List(-7.47568176e-02, 1.09440908e+00),
      List(-2.15941266e-03, -1.28416671e-03),
      List(-1.16584891e-01, 2.37544793e-01),
      List(-6.42279094e-02, 5.11934442e-01),
      List( 1.27378234e+00, -4.10054152e-01),
      List(-5.56169141e-04, -2.85606358e-03),
      List(-2.83901831e-01, -3.52518356e+00),
      List(-2.03420024e-03, -2.27249587e-03),
      List(-5.58870610e-02, -1.60068003e-02),
      List(-2.97422212e-02, 7.45456608e-01),
      List( 1.17329619e-03, -5.43001556e-04),
      List(-5.69175678e-04, 3.15156854e-03),
      List( 4.03951183e-04, 2.28270493e-04)))

    val weights2 = Matrix(List(
    List(-5.23724322e-03, 5.03378156e-03, -6.91668988e-04,
        -1.43404597e-02, -2.98714016e-03, 2.10825296e-02,
        7.35857898e-02, -1.88938073e-02, -2.98483674e-03,
        4.23756054e-02, -7.23551621e-04, 7.25170499e-02,
        -7.35045780e-02, -1.47583861e-03, 3.26109165e-03, 1.30853499e-04),
      List(1.18994529e+00, 1.23091048e-01, -5.42228307e-01,
          -1.02062930e+00, 1.75830472e-03, -3.49586781e-01,
          -5.58596388e-01, -1.33754998e-01, -8.94186323e-04,
          1.42948917e+00, 3.18854989e-03, -2.73745511e-01,
          1.12164911e+00, 5.97657747e-04, 1.22377049e-03, 1.86444362e-03),
      List(-2.01827136e-03, 3.05663369e-03, -1.58354357e-03,
          -1.19317093e-03, 1.74738263e-03, 4.19804946e-04,
          4.09908338e-04, -2.36775498e-03, 8.92821495e-04,
          -3.35601939e-03, -2.24410256e-03, 2.37274312e-03,
          1.98176341e-03, 1.72085334e-03, -2.07468744e-03, 4.73936754e-04),
      List(-1.61369877e-03, 5.95742259e-04, 2.72481127e-03,
          1.86235890e-04, 2.91159927e-03, -3.31306126e-03,
          -3.35498660e-03, -3.31355054e-03, -1.99481843e-03,
          -1.82833640e-03, -2.57190448e-03, 2.79023999e-03,
          7.71089772e-04, 2.46620755e-03, -5.04503331e-04, -2.36012533e-04),
      List(-1.15243642e-03, 2.75402987e-03, -5.77621585e-04,
          2.65636775e-03, 1.45588561e-03, 2.07491381e-03,
          -3.38377218e-03, -1.62445348e-02, 1.73966951e-03,
          6.99978823e-05, -2.51168234e-03, -2.76136675e-04,
          -3.07035958e-03, 1.75568330e-03, -1.07821872e-03, -1.08274301e-03),
      List(-2.41520076e-01, -3.18765648e-02, 1.81223826e-01,
          3.11597980e-01, -2.58607315e-03, 1.05335226e-01,
          1.99925698e-01, 1.85166782e-01, -2.98344885e-03,
          -7.18476398e-01, 3.89716341e-05, 8.48886989e-02,
          -3.44462627e-01, 1.36319124e-03, 2.04859258e-03, -2.97193481e-03),
      List(-1.03974195e-01, -1.30986514e-02, 7.08504400e-02,
          1.15751077e-01, -3.30457582e-03, 4.08367383e-02,
          7.62724172e-02, 2.12494425e-01, -1.64970663e-03,
          -1.22831615e-01, -3.29325585e-03, 3.26113269e-02,
          -1.23027703e-01, 2.50864418e-03, 2.61545760e-03, -8.40225188e-04),
      List(-2.43150157e-03, 2.11571712e-03, 8.37609027e-04,
          8.49064752e-04, -3.26100173e-03, -7.71834348e-04,
          -2.04217753e-03, 4.73891647e-04, 2.09756903e-03,
          3.89933366e-04, 3.08329572e-03, -1.47485732e-03,
          2.88115825e-03, -1.22045375e-03, 2.77956913e-03, -2.89335559e-03),
      List(9.48428214e-03, -7.19908698e-04, -2.65163187e-03,
          -3.33251241e-03, -3.24657879e-03, 2.50219103e-03,
          -1.67423611e-03, -1.25788374e-02, -2.11003486e-03,
          -1.37949388e-03, 2.25147500e-03, -6.70831926e-05,
          -3.43864002e-03, 1.17550751e-03, 1.11997023e-03, 3.32314047e-03),
      List(-5.11439082e-01, -6.29753401e-02, 4.14837832e-01,
          6.48428928e-01, -1.74678206e-03, 2.23011570e-01,
          4.59284960e-01, 5.69772450e-01, 9.59883327e-04,
          -1.31597974e+00, -3.33803423e-03, 1.75317169e-01,
          -7.06893801e-01, -3.38125566e-04, -1.58161383e-03, -2.19139958e-03),
      List(9.42282931e-03, 2.89049680e-03, -4.59558235e-04,
          1.31390801e-03, 2.47800547e-03, 3.91664527e-04,
          -2.18006321e-03, 2.80538365e-03, 1.69801153e-04,
          3.44294729e-02, -2.22868100e-03, -3.33449443e-03,
          -8.49474758e-04, -8.11464848e-04, -1.70441397e-03, 2.14646546e-03),
      List(-1.41126611e+00, 1.95659224e-02, 7.35424851e-02,
          7.77444989e-02, 2.67267000e-04, 3.30201233e-02,
          1.42975144e-01,-8.60897954e-01, 2.92387432e-03,
          -1.98652517e+00, -1.15587521e-03, 1.51685288e-02,
          -2.79239585e-02, -6.13868904e-04, 2.44269659e-03, 2.58079132e-03),
      List(8.21864748e-02, 1.08404790e-02, -1.78638398e-01,
          -9.83961583e-02, 3.63563863e-04, -2.46242904e-02,
          6.30341785e-02, 2.16325323e-01, -1.62918434e-03,
          5.50079418e-02, -2.53978014e-03, 1.26367253e-01,
          -5.85789579e-02, -6.78097719e-04, 1.90754760e-04, 1.29681340e-03),
      List(-2.60481184e-02, 3.91052943e-02, -6.85492980e-01,
          -1.22829794e+00, 2.35413507e-03, -4.19126625e-01,
          -8.19702141e-01, 5.62753368e-01, -2.98740915e-03,
          2.57170320e+00, 2.13016692e-03, -3.29529050e-01,
          1.35093770e+00, -1.25691236e-03, 2.07971859e-03, 4.78317128e-04),
      List(6.15555135e-01, 4.60010532e-03, -5.71904131e-01,
          -8.57594119e-01, -2.58674523e-03, -2.96386651e-01,
          -4.88778960e-01, -1.23385828e+00, 1.61791534e-03,
          3.25172116e-01, -1.06864877e-03, -3.99704101e-01,
          1.03290054e+00, 8.39148353e-04, 4.98924993e-04, -2.33347060e-03),
      List(-7.29779880e-01, -8.91028918e-02, 4.28292341e-01,
          9.58351782e-01, -1.51983770e-03, 3.25420030e-01,
          5.58716467e-01, -4.78830637e-01, 1.87335080e-03,
          -2.39126750e+00, 3.02917801e-03, 2.61623070e-01,
          -1.05204472e+00, -4.80095073e-04, 1.60467668e-03, -2.27958648e-04)))

    val weights3 = Matrix(List(
      List(-3.29849489e-02, 8.35244610e-01, -1.87788824e-04,
          -1.57334338e-04, -3.36449601e-03, 5.69718037e-01,
          1.90104837e-01, 1.06089872e-03, -6.58504086e-03,
          1.63385901e+00, -3.88565278e-02, 2.76664811e+00,
          2.24830815e-01, -1.99009145e+00, -3.37098328e-01, -2.70094654e+00)))

    val bias1 = Vector(List(-0.19842708, -0.82484856, -0.33017571, -0.13656071,
      -0.00331553, -0.16418985, -0.12052034, 1.04113267, -0.00151116,
      0.18731697, -0.00273092, -0.51978225, -0.09866906, -0.00142052,
      -0.0014607 , -0.00254644))

    val bias2 = Vector(List(1.31573986e-01, 6.11254651e-02, 4.32920216e-04,
      -1.29211182e-03, -1.51416872e-04,7.36684685e-02, -3.50503586e-02,
      -2.67816287e-03, -6.93123059e-03, -1.56053484e-02, 1.29431622e-01,
      1.03422374e+00, 9.46628441e-02, -6.36501886e-01, 4.46348738e-01, 5.74399455e-01))

    val bias3 = Vector(List(-0.30043814))

    val layer1 = relu(weights1 * x + bias1)
    val layer2 = relu(weights2 * layer1 + bias2)
    val layer3 = linear(weights3 * layer2 + bias3)

    layer3

  } ensuring(res => res +/- 1e-3)
  // ensuring(res => res +/- 1e-5)
}