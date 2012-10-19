package common

final class DefaultEnsuring[+T](default: T) {

    require(default != null)

    /**
     * Use this guard when you are not able to use argument's default values, otherwise do NOT use this.
     */
    def ?:[A >: T](attempt: A) = if (attempt == null) default else attempt

}

final object DefaultEnsuring {

    implicit def toDefaultEnsuring[T](d: T) = new DefaultEnsuring(d)

}
