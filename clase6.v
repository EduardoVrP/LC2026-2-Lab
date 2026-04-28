Section Ejemplos.
    Variables p q r s t x l m:Prop.
    (*Definimos las cosas que usaremos
    para las pruebas*)

    Theorem inn: p -> ~ ~ p.
    Proof.
        intro.
        unfold not.
        intro.
        exact(H0 H).
    Qed.

    Theorem deM3: ~p \/ ~ q -> ~(p /\ q).
    Proof.
        intros.
        unfold not.
        intro.
        destruct H0.
        destruct H.
        -  exact (H H0).
        -  exact (H H1).
    Qed.

    Lemma MP: (p -> q) -> p -> q.
    Proof.
        intros. 
        apply H.
        trivial.
    Qed.

    Lemma ConjI: p -> q -> p /\ q.
    Proof.
        intros.
        split.
        trivial.
        trivial.
    Qed.

    Theorem defimp: (~p \/ q) -> p -> q.
    Proof.
        unfold not.
        intros.
        destruct H.
        - exfalso.
        - apply H.
        - trivial
        intuition.
        - trivial.
    Qed.

    Lemma DisyE: (p -> r) -> (q -> r) -> p \/ q -> r.
    Proof.
        intros.
        destruct H1.
        trivial.
        trivial.
    Qed.

    Theorem Imptrans : (p -> q) -> (q -> s) -> (p -> s).
    Proof.
        intro h1.
        intro h2.
        intro h3.
        apply h2.
        apply h1.
        exact h3.
    Qed.

    Theorem ImpExp3: (p /\ q -> r) -> p -> q -> r.
    Proof.
        intros.
        apply H.
        split.
        - exact H0.
        - exact H1.
    Qed.

    Theorem Disy2: q \/ p -> p \/ q.
    Proof.
        intros.
        destruct H.
        intros.
        right.
        assumption.
        intros.
        left.
        exact H0.
    Qed.

    Theorem DilemaC2 : (p -> q) -> (r -> s) -> p \/ r -> q \/ s. 
    Proof.
        intros.
        destruct H1.
        left.
        apply H. 
        trivial.
        right.
        exact (H0 H1).
    Qed.


    Theorem Distrib : p \/ (q /\ r) -> (p \/ q) /\ (p \/ r).
    Proof.
        intros.
        split.
        - destruct H.
        + left.
            exact H.
        + right.
            destruct H.
            exact H.
        - destruct H.
        + left.
            exact H.
        + destruct H.
            right. exact H0.
    Qed.  