use aiken/collection/dict as dict
use aiken/collection/list as list
use aiken/option as option
use cardano/address.{Script}
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{
  InlineDatum, Input, OutputReference, Transaction,
}

pub type ImmutableSettings {
  policyId: PolicyId,
  assetName: AssetName,
}

@list
pub type MutableSettings {
  vFunction: PolicyId,
  incentive: Int,
}

@list
pub type ReserveDatum {
  immutableSettings: ImmutableSettings,
  mutableSettings: MutableSettings,
  stats: Int,
}

@list
pub type VersionedGenericDatum<a> {
  data: a,
  appendix: Data,
  version: Int,
}

@list
pub type Wiksa {
  data: ReserveDatum,
  appendix: Data,
  version: Int,
}


type ReserveRedeemer {
  DepositToReserve
  TransferToIlliquidCirculationSupply
  UpdateReserve
  Handover
}

@list
pub type VersioningDatum {
  scriptId: Int,
  currencySymbol: PolicyId,
}

validator reserve(version_oracle_config: PolicyId) {
  spend(
    _datum: Option<Wiksa>,
    redeemer: Data,
    utxo: OutputReference,
    self: Transaction,
  ) {
    trace @"HELLO5"
    expect reserve_redeemer: ReserveRedeemer = redeemer

    expect [reserveAuthTokenReferenceInput] =
      self.reference_inputs
        |> list.filter(
            fn(input) {
              let output = input.output
              let has_token =
                assets.to_dict(output.value)
                  |> dict.get(version_oracle_config)
                  |> option.and_then(dict.get(_, "Version oracle"))
                  |> option.is_some()

              when output.datum is {
                InlineDatum(data) -> {
                    if data is datum: VersioningDatum {
                    has_token && datum.scriptId == 29
                  } else {
                    False
                  }
                  }
                _ -> False
              }
            },
          )




    let reserve_auth_script_hash =
      reserveAuthTokenReferenceInput.output.reference_script
        |> option.or_else(fail @"missing script 1")




    let burns_reserve_auth =
      -1 == (
        self.mint
          |> assets.to_dict
          |> dict.get(reserve_auth_script_hash)
          |> option.and_then(dict.get(_, ""))
          |> option.or_else(0)
      )

    expect Some(input) =
      self.inputs
        |> transaction.find_input(utxo)

    let input_utxo = input.output
    expect Script(reserve_script_hash) = input_utxo.address.payment_credential

    let reserve_outputs =
      self.outputs
        |> transaction.find_script_outputs(reserve_script_hash)

    expect InlineDatum(input_datum) = input_utxo.datum
    expect reserve_input_datum: Wiksa = input_datum
    let reserve_policy_id = reserve_input_datum.data.immutableSettings.policyId
    let reserve_token_name = reserve_input_datum.data.immutableSettings.assetName

    let input_reserve_tokens =
      input_utxo.value
        |> assets.to_dict
        |> dict.get(reserve_policy_id)
        |> option.and_then(dict.get(_, reserve_token_name))
        |> option.or_else(0)



    let mintedVFunctionTokens =
      self.mint
        |> assets.to_dict
        |> dict.get(reserve_input_datum.data.mutableSettings.vFunction)
        |> option.and_then(dict.get(_, ""))
        |> option.or_else(0)

    let transfered_to_this_point = reserve_input_datum.data.stats



    when reserve_redeemer is {
      DepositToReserve -> {
        expect [output_utxo] = reserve_outputs
        let datum_does_not_change = input_utxo.datum == output_utxo.datum
        let output_reserve_tokens =
          output_utxo.value
            |> assets.to_dict
            |> dict.get(reserve_policy_id)
            |> option.and_then(dict.get(_, reserve_token_name))
            |> option.or_else(0)

        expect [governanceReferenceInput] =
          self.reference_inputs
            |> list.filter(
                fn(input) {
                  let output = input.output
                  let has_token =
                    assets.to_dict(output.value)
                      |> dict.get(version_oracle_config)
                      |> option.and_then(dict.get(_, "Version oracle"))
                      |> option.is_some()

                  when output.datum is {
                    InlineDatum(data) -> {
                        if data is datum: VersioningDatum {
                        has_token && datum.scriptId == 32
                      } else {
                        False
                      }
                      }
                    _ -> False
                  }
                },
              )

        let governanceScriptHash =
          governanceReferenceInput.output.reference_script
            |> option.or_else(fail @"missing script 2")
        let mintsGovernance =
          self.mint
            |> assets.to_dict
            |> dict.get(governanceScriptHash)
            |> option.is_some()
        mintsGovernance? && datum_does_not_change? && (output_reserve_tokens >= input_reserve_tokens)?
      }
      TransferToIlliquidCirculationSupply -> {
        expect [output_utxo] = reserve_outputs
        expect InlineDatum(output_datum) = output_utxo.datum
        expect reserve_output_datum: Wiksa = output_datum
        let output_reserve_tokens =
          output_utxo.value
            |> assets.to_dict
            |> dict.get(reserve_policy_id)
            |> option.and_then(dict.get(_, reserve_token_name))
            |> option.or_else(0)
        let asset_change_by_correct_amount =
          transfered_to_this_point - mintedVFunctionTokens == output_reserve_tokens - input_reserve_tokens
        let datum_change_only_by_stats =
          reserve_output_datum.data.immutableSettings == reserve_input_datum.data.immutableSettings && reserve_output_datum.data.mutableSettings == reserve_input_datum.data.mutableSettings

        expect [illiquidCirculationSupplyReferenceInput] =
          self.reference_inputs
            |> list.filter(
                fn(input) {
                  let output = input.output
                  let has_token =
                    assets.to_dict(output.value)
                      |> dict.get(version_oracle_config)
                      |> option.and_then(dict.get(_, "Version oracle"))
                      |> option.is_some()

                  when output.datum is {
                    InlineDatum(data) -> {
                        if data is datum: VersioningDatum {
                        has_token && datum.scriptId == 30
                      } else {
                        False
                      }
                      }
                    _ -> False
                  }
                },
              )
        let illiquid_circulation_supply_script_hash =
          illiquidCirculationSupplyReferenceInput.output.reference_script
            |> option.or_else(fail @"missing script 3")
        expect [output_ics_utxo] =
                  self.outputs
                    |> list.filter(
                        fn(output) {
                          let addr = output.address
                          addr.payment_credential == Script(
                            illiquid_circulation_supply_script_hash,
                          )
                        },
                      )
        let output_ics_reserve_amount =
          output_ics_utxo.value
            |> assets.to_dict
            |> dict.get(reserve_policy_id)
            |> option.and_then(dict.get(_, reserve_token_name))
            |> option.or_else(0)

        let illiquid_inputs_amount =
          self.inputs
            |> list.filter(
                fn(input) {
                  let output = input.output
                  output.address.payment_credential == Script(
                    illiquid_circulation_supply_script_hash,
                  )
                },
              )
            |> list.foldr(
                0,
                fn(input: Input, acc: Int) {
                  acc + (
                    assets.to_dict(input.output.value)
                      |> dict.get(reserve_policy_id)
                      |> option.and_then(dict.get(_, reserve_token_name))
                      |> option.or_else(0)
                  )
                },
              )

        let correct_amount_transferred_to_ics =
          output_ics_reserve_amount + reserve_input_datum.data.mutableSettings.incentive == mintedVFunctionTokens - transfered_to_this_point + illiquid_inputs_amount

        datum_change_only_by_stats? && asset_change_by_correct_amount? && correct_amount_transferred_to_ics?
      }
      UpdateReserve -> {
        expect [output_utxo] = reserve_outputs
        expect InlineDatum(output_datum) = output_utxo.datum
        expect reserve_output_datum: Wiksa = output_datum
        let output_reserve_tokens =
          output_utxo.value
            |> assets.to_dict
            |> dict.get(reserve_policy_id)
            |> option.and_then(dict.get(_, reserve_token_name))
            |> option.or_else(0)
        let datum_change_only_by_mutable_settings =
          reserve_output_datum.data.immutableSettings == reserve_input_datum.data.immutableSettings && reserve_output_datum.data.stats == reserve_input_datum.data.stats

        expect [governanceReferenceInput] =
          self.reference_inputs
            |> list.filter(
                fn(input) {
                  let output = input.output
                  let has_token =
                    assets.to_dict(output.value)
                      |> dict.get(version_oracle_config)
                      |> option.and_then(dict.get(_, "Version oracle"))
                      |> option.is_some()

                  when output.datum is {
                    InlineDatum(data) -> {
                        if data is datum: VersioningDatum {
                        has_token && datum.scriptId == 32
                      } else {
                        False
                      }
                      }
                    _ -> False
                  }
                },
              )

        let governanceScriptHash =
          governanceReferenceInput.output.reference_script
            |> option.or_else(fail @"missing script 4")

        let mintsGovernance =
          self.mint
            |> assets.to_dict
            |> dict.get(governanceScriptHash)
            |> option.is_some()

        mintsGovernance? && datum_change_only_by_mutable_settings? && (output_reserve_tokens == input_reserve_tokens)?
      }
      Handover -> {
        // No continuing output at reserve address
        expect [] = reserve_outputs

        expect [illiquidCirculationSupplyReferenceInput] =
          self.reference_inputs
            |> list.filter(
                fn(input) {
                  let output = input.output
                  let has_token =
                    assets.to_dict(output.value)
                      |> dict.get(version_oracle_config)
                      |> option.and_then(dict.get(_, "Version oracle"))
                      |> option.is_some()

                  when output.datum is {
                    InlineDatum(data) -> {
                        if data is datum: VersioningDatum {
                        has_token && datum.scriptId == 30
                      } else {
                        False
                      }
                      }
                    _ -> False
                  }
                },
              )

        let illiquid_circulation_supply_script_hash =
          illiquidCirculationSupplyReferenceInput.output.reference_script
            |> option.or_else(fail @"missing script 5")
        expect [output_ics_utxo] =
                  self.outputs
                    |> list.filter(
                        fn(output) {
                          let addr = output.address
                          addr.payment_credential == Script(
                            illiquid_circulation_supply_script_hash,
                          )
                        },
                      )
        let output_ics_reserve_amount =
          output_ics_utxo.value
            |> assets.to_dict
            |> dict.get(reserve_policy_id)
            |> option.and_then(dict.get(_, reserve_token_name))
            |> option.or_else(0)

        let illiquid_inputs_amount =
          self.inputs
            |> list.filter(
                fn(input) {
                  let output = input.output
                  output.address.payment_credential == Script(
                    illiquid_circulation_supply_script_hash,
                  )
                },
              )
            |> list.foldr(
                0,
                fn(input: Input, acc: Int) {
                  acc + (
                    assets.to_dict(input.output.value)
                      |> dict.get(reserve_policy_id)
                      |> option.and_then(dict.get(_, reserve_token_name))
                      |> option.or_else(0)
                  )
                },
              )

        let all_reserve_tokens_transfered_to_ics =
              illiquid_inputs_amount + input_reserve_tokens == output_ics_reserve_amount

        expect [governanceReferenceInput] =
          self.reference_inputs
            |> list.filter(
                fn(input) {
                  let output = input.output
                  let has_token =
                    assets.to_dict(output.value)
                      |> dict.get(version_oracle_config)
                      |> option.and_then(dict.get(_, "Version oracle"))
                      |> option.is_some()

                  when output.datum is {
                    InlineDatum(data) -> {
                        if data is datum: VersioningDatum {
                        has_token && datum.scriptId == 32
                      } else {
                        False
                      }
                      }
                    _ -> False
                  }
                },
              )

        let governanceScriptHash =
          governanceReferenceInput.output.reference_script
            |> option.or_else(fail @"missing script 6")

        let mintsGovernance =
          self.mint
            |> assets.to_dict
            |> dict.get(governanceScriptHash)
            |> option.is_some()

        mintsGovernance? && burns_reserve_auth? && all_reserve_tokens_transfered_to_ics?
      }
    }
  }

  else(_) {
    fail
  }

  // // If needs be, remove any of unneeded handlers above, and use:
  //
  // else(_ctx: ScriptContext) {
  //   todo @"fallback logic if none of the other purposes match"
  // }
  //
  // // You will also need an additional import:
  // //
  // // use cardano/script_context.{ScriptContext}
}
