import shapeless._
object removeField {

  import labelled._
  import ops.record._
  import ops.hlist.Prepend

  trait RemoveField[T] {
    type Out <: HList
    def removeField(t: T, field:Witness): Out
  }

  object RemoveField {
    type Aux[T, R <: HList] = RemoveField[T]{type Out = R}

    implicit def generic[F, G, R <: HList](implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[RemoveField.Aux[G, R]]): RemoveField.Aux[F,R] =
      new RemoveField[F] {
        type Out = R
        def removeField(f: F, field:Witness) = sg.value.removeField(gen.to(f), field)
      }

    implicit def product: RemoveField.Aux[HNil, HNil] =
      new RemoveField[HNil] {
        type Out = HNil
        def removeField(p: HNil, field:Witness) = HNil
      }

      implicit def product[K <: Symbol, V, T <: HList, RV <: HList, RT <: HList, R <: HList]
      (implicit
        key: Witness.Aux[K],
        selector: Selector.Aux[FieldType[K, V] :: T, K, Symbol],
        sv: Lazy[RemoveField.Aux[V, RV]],
        st: Lazy[RemoveField.Aux[T,RT]],
        prepend: Prepend.Aux[RV, RT, R]
      ): RemoveField.Aux[FieldType[K, V] :: T, R] =
        new RemoveField[FieldType[K, V] :: T] {
          type Out =  R
          def removeField(p: FieldType[K, V] :: T, field:Witness) = {
            if (selector(p) == selector(p)) sv.value.removeField(p.head, field)
            else p.head :: st.value.removeField(p.tail, field)
          }
        }

        implicit def cnil: RemoveField.Aux[CNil, HNil] =
      new RemoveField[CNil] {
        type Out = HNil
        def removeField(p: CNil, field:Witness) = HNil
      }

    implicit def coproduct[K <: Symbol, V, T <: Coproduct, RV <: HList, RT <: HList, R <: HList]
      (implicit
        key: Witness.Aux[K],
        sv: Lazy[RemoveField.Aux[V, RV]],
        st: Lazy[RemoveField.Aux[T, RT]],
        prepend: Prepend.Aux[RV, RT, R]
      ): RemoveField.Aux[FieldType[K, V] :+: T, R] =
        new RemoveField[FieldType[K, V] :+: T] {
          type Out = R
          def removeField(c: FieldType[K, V] :+: T, field:Witness): HList =
            c match {
              case Inl( head ) => sv.value.removeField(head, field )
              case Inr( tail ) => st.value.removeField(tail, field )
            }
        }
  }

  implicit class RemoveFieldOps[T](x: T)(implicit removeFieldT: RemoveField[T]) {
    def removeField(field:Witness): HList = removeFieldT.removeField(x, field)
  }

  sealed trait Animal
  case class Cat(name: String, fish: Int) extends Animal
  case class Dog(name: String, bones: Int) extends Animal

  val felix:Cat = Cat("Felix", 1)
  val tigger = Dog("Tigger", 2)
}

