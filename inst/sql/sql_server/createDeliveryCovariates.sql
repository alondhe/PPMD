with sb
as
(
  select A.subject_id as row_id, 1 as covariate_id, 1 as covariate_value, 
  row_number() over (PARTITION BY B.person_id ORDER BY B.episode_end_date, B.episode) as ordinal
  from @cohortTable A
  join @resultsDatabaseSchema.pregnancy_episodes B on A.subject_id = B.person_id
    and A.cohort_start_date = B.episode_end_date
    and B.episode_end_date >= '2009-01-01'
    and B.episode_end_date <= '2012-12-31'
  where B.outcome = 'SB' 
),
ect
as
(
  select A.subject_id as row_id, 2 as covariate_id, 1 as covariate_value,
  row_number() over (PARTITION BY B.person_id ORDER BY B.episode_end_date, B.episode) as ordinal
  from @cohortTable A
  join @resultsDatabaseSchema.pregnancy_episodes B on A.subject_id = B.person_id
    and A.cohort_start_date = B.episode_end_date
    and B.episode_end_date >= '2009-01-01'
    and B.episode_end_date <= '2012-12-31'
  where B.outcome = 'ECT'
),
lb_deliv
as
(
  select A.subject_id as row_id, 3 as covariate_id, 1 as covariate_value,
  row_number() over (PARTITION BY B.person_id ORDER BY B.episode_end_date, B.episode) as ordinal
  from @cohortTable A
  join @resultsDatabaseSchema.pregnancy_episodes B on A.subject_id = B.person_id
    and A.cohort_start_date = B.episode_end_date
    and B.episode_end_date >= '2009-01-01'
    and B.episode_end_date <= '2012-12-31'
  where B.outcome = 'LB/DELIV'
)
select row_id, covariate_id, covariate_value from sb
where ordinal = 1
union all
select row_id, covariate_id, covariate_value from ect
where ordinal = 1
union all 
select row_id, covariate_id, covariate_value from lb_deliv
where ordinal = 1;
