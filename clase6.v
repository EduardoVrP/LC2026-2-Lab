Section Ejemplos.

  Variables p q r s t x l m : Prop.

  (* Definimos las cosas que usaremos para las pruebas *)

  Theorem inn : p -> ~~p.
  Proof.
    intro H.
    unfold not.
    intro H0.
    exact (H0 H).
  Qed.

  Theorem deM3 : ~p \/ ~q -> ~(p /\ q).
  Proof.
    intros H.
    unfold not.
    intro H0.
    destruct H0 as [Hp Hq].
    destruct H as [Hnp | Hnq].
    - exact (Hnp Hp).
    - exact (Hnq Hq).
  Qed.

  Lemma MP : (p -> q) -> p -> q.
  Proof.
    intros H Hp.
    apply H.
    exact Hp.
  Qed.

  Lemma ConjI : p -> q -> p /\ q.
  Proof.
    intros Hp Hq.
    split.
    - exact Hp.
    - exact Hq.
  Qed.

  Theorem defimp : (~p \/ q) -> p -> q.
  Proof.
    intros H Hp.
    destruct H as [Hnp | Hq].
    - exfalso.
      apply Hnp.
      exact Hp.
    - exact Hq.
  Qed.

  Lemma DisyE : (p -> r) -> (q -> r) -> p \/ q -> r.
  Proof.
    intros Hp Hr Hpq.
    destruct Hpq as [Hp' | Hq'].
    - apply Hp. exact Hp'.
    - apply Hr. exact Hq'.
  Qed.

  Theorem Imptrans : (p -> q) -> (q -> s) -> (p -> s).
  Proof.
    intros Hpq Hqs Hp.
    apply Hqs.
    apply Hpq.
    exact Hp.
  Qed.

  Theorem ImpExp3 : (p /\ q -> r) -> p -> q -> r.
  Proof.
    intros H Hp Hq.
    apply H.
    split.
    - exact Hp.
    - exact Hq.
  Qed.

  Theorem Disy2 : q \/ p -> p \/ q.
  Proof.
    intro H.
    destruct H as [Hq | Hp].
    - right. exact Hq.
    - left. exact Hp.
  Qed.

  Theorem DilemaC2 : (p -> q) -> (r -> s) -> p \/ r -> q \/ s.
  Proof.
    intros Hpq Hrs Hpr.
    destruct Hpr as [Hp | Hr].
    - left. apply Hpq. exact Hp.
    - right. apply Hrs. exact Hr.
  Qed.

  Theorem Distrib : p \/ (q /\ r) -> (p \/ q) /\ (p \/ r).
  Proof.
    intro H.
    split.
    - destruct H as [Hp | [Hq Hr]].
      + left. exact Hp.
      + right. exact Hq.
    - destruct H as [Hp | [Hq Hr]].
      + left. exact Hp.
      + right. exact Hr.
  Qed.

End Ejemplos.