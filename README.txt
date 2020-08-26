
Sample sequence of invocations from sbt

Upload to Evernote
runMain fm.wrk.grafik.note.Notes sbtb2020.tsv sbtb202

Verify .enex merge
runMain fm.wrk.grafik.talk.ShowTalks sbtb2020.tsv accepted.enex

Create Cards
runMain fm.wrk.grafik.cards.CreateOneProject sbtb2020.tsv sbtb2020 A accepted.enex

Sched Sessions
runMain fm.wrk.grafik.sched.Sched /data/sbtb2020/ sbtb2020.tsv 2020-11-12 2020-11-13 sbtb2020talks0.xlsx sbtb2020talks.xlsx ab sbtb2020a sbtb2020b

Sched Speakers
runMain fm.wrk.grafik.sched.Speakers /data/sbtb2020/ sbtb2020.tsv accepted.enex sbtb2020speakers0.xlsx sbtb2020speakers.xlsx 6