mod l_one {
	use anyhow::Result;
	use warren_abstract_machine::{l_one::L1, parser::ParseAs, solve};

	#[test]
	fn test_simple() -> Result<()> {
		assert_eq!(
			solve::<L1>("p(a).".parse_as()?, "p(X).".parse_as()?)?,
			"{ X -> a }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("test(f(c)).".parse_as()?, "test(C).".parse_as()?)?,
			"{ C -> f(c) }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("a(X, X).".parse_as()?, "a(X, a).".parse_as()?)?,
			"{ X -> a }".parse_as()?
		);

		Ok(())
	}

	#[test]
	fn test_deep() -> Result<()> {
		assert_eq!(
			solve::<L1>(
				"test(a(b(c(d(e(f(g(X))))))), X, a(b(c(d(e(f(g(constant)))))))).".parse_as()?,
				"test(X, Y, Y).".parse_as()?
			)?,
			"{ X -> a(b(c(d(e(f(g(a(b(c(d(e(f(g(constant)))))))))))))), Y -> a(b(c(d(e(f(g(constant))))))) }"
				.parse_as()?
		);

		Ok(())
	}

	#[test]
	fn test_broad() -> Result<()> {
		assert_eq!(
			solve::<L1>(
				"f(AA,AB,AC,AD,AE,AR,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ,BA,BB,BC,BD,BE,BR,BF,BG,BH,BI,BJ,BK,BL,BM,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ,CA,CB,CC,CD,CE,CR,CF,CG,CH,CI,CJ,CK,CL,CM,CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ).".parse_as()?,
				"f(AA,AB,AC,AD,AE,AR,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ,BA,BB,BC,BD,BE,BR,BF,BG,BH,BI,BJ,BK,BL,BM,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ,CA,CB,CC,CD,CE,CR,CF,CG,CH,CI,CJ,CK,CL,CM,CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ).".parse_as()?
			)?,
			"{ AA -> ?1, AB -> ?2, AC -> ?3, AD -> ?4, AE -> ?5, AF -> ?6, AG -> ?7, AH -> ?8, AI -> ?9, AJ -> ?10, AK -> ?11, AL -> ?12, AM -> ?13, AN -> ?14, AO -> ?15, AP -> ?16, AQ -> ?17, AR -> ?18, AS -> ?19, AT -> ?20, AU -> ?21, AV -> ?22, AW -> ?23, AX -> ?24, AY -> ?25, AZ -> ?26, BA -> ?27, BB -> ?28, BC -> ?29, BD -> ?30, BE -> ?31, BF -> ?32, BG -> ?33, BH -> ?34, BI -> ?35, BJ -> ?36, BK -> ?37, BL -> ?38, BM -> ?39, BN -> ?40, BO -> ?41, BP -> ?42, BQ -> ?43, BR -> ?44, BS -> ?45, BT -> ?46, BU -> ?47, BV -> ?48, BW -> ?49, BX -> ?50, BY -> ?51, BZ -> ?52, CA -> ?53, CB -> ?54, CC -> ?55, CD -> ?56, CE -> ?57, CF -> ?58, CG -> ?59, CH -> ?60, CI -> ?61, CJ -> ?62, CK -> ?63, CL -> ?64, CM -> ?65, CN -> ?66, CO -> ?67, CP -> ?68, CQ -> ?69, CR -> ?70, CS -> ?71, CT -> ?72, CU -> ?73, CV -> ?74, CW -> ?75, CX -> ?76, CY -> ?77, CZ -> ?78 }".parse_as()?
		);

		Ok(())
	}

	#[test]
	fn test_wambook() -> Result<()> {
		assert_eq!(
			solve::<L1>("p(f(X), h(Y, f(a)), Y).".parse_as()?, "p(Z, h(Z,W), f(W)).".parse_as()?)?,
			"{ W -> f(a), Z -> f(f(a)) }".parse_as()?
		);

		Ok(())
	}

	#[test]
	fn test_unbound() -> Result<()> {
		assert_eq!(
			solve::<L1>("p(A, B, A).".parse_as()?, "p(X, Y, Z).".parse_as()?)?,
			"{ X -> ?1, Y -> ?2, Z -> ?1 }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("p(A, B, B).".parse_as()?, "p(X, Y, Z).".parse_as()?)?,
			"{ X -> ?1, Y -> ?2, Z -> ?2 }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("p(A, A, B).".parse_as()?, "p(X, Y, Z).".parse_as()?)?,
			"{ X -> ?1, Y -> ?1, Z -> ?2 }".parse_as()?
		);

		Ok(())
	}

	#[test]
	fn test_multiple_facts() -> Result<()> {
		assert_eq!(
			solve::<L1>("a(x).".parse_as()?, "a(X).".parse_as()?)?,
			"{ X -> x }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("a(x). b(y).".parse_as()?, "a(X).".parse_as()?)?,
			"{ X -> x }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("a(x). b(y).".parse_as()?, "b(Y).".parse_as()?)?,
			"{ Y -> y }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("a(x). a(x, y).".parse_as()?, "a(X).".parse_as()?)?,
			"{ X -> x }".parse_as()?
		);

		assert_eq!(
			solve::<L1>("a(x). a(x, y).".parse_as()?, "a(X, Y).".parse_as()?)?,
			"{ X -> x, Y -> y }".parse_as()?
		);

		Ok(())
	}
}
