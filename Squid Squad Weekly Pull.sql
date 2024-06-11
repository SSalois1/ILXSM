SELECT land_date, area_code, tally_vessel_seq, sample_seq, organism_id, param_value_num, param_type, param_descr, unit_measure, vessel_name, vtr_serial_num
      FROM(SELECT t.tally_no, v.tally_vessel_seq, s.sample_seq, o.organism_id, o.opv_seq, o.param_value_num, 
                  o.param_type, o.param_descr, o.unit_measure, v.land_date, v.area_code, v.vessel_name, v.vtr_serial_num
      FROM BSM.BSM_Tally_View t,
           BSM.BSM_Tally_Vessels_View v,
           BSM.BSM_Samples_View s,
           BSM.BSM_Organism_Parameter_View o
      WHERE v.tally_no = t.tally_no
        and s.tally_no = v.tally_no
        and s.tally_vessel_seq = v.tally_vessel_seq
        and o.tally_no = s.tally_no
        and o.tally_vessel_seq = s.tally_vessel_seq
        and o.sample_seq = s.sample_seq 
        and t.sample_source_code = '03'
        and s.species_pk like '082521%'
        and o.param_type in ('ML','WT','OW')
        and land_date BETWEEN '21-Sep-23' AND '27-Sep-23')
ORDER BY tally_no, tally_vessel_seq, sample_seq, organism_id, opv_seq, param_type;
