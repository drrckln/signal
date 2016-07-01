library(dplyr)
df = read.csv("~/repos/signal/gss/gss_old.csv")

df = df %>% select(-agekdbrn, -ballot, -consent, -dateintv, -family16, -form, -formwt, -gender1, -gender2,
                   -hefinfo, -hhtype, -hispanic, -id, -intage, -intethn, -inthisp, -intid, -intrace1, -intsex,
                   -old1, -old2, -oversamp, -phase, -relate1, -relate2, -relhh1, -relhh2, -relhhd1, -relhhd2,
                   -sampcode, -version, -vpsu, -vstrat, -worda, -wordb, -wordd, -worde, -wordf, -wordg, -wordi,
                   -wordj, -wtss, -wtssnr, -wtssall, -year, -X)

df$cshutyp12[df$cshutyp12 == 9] = NA
df$eqwlth[df$eqwlth == 98] = NA
df$fund[is.na(df$fund)] = 9
df$fund16[is.na(df$fund16)] = 9
df$helpful = ifelse(df$helpful == 2, 3, ifelse(df$helpful == 3, 2, df$helpful))
df$nummen[df$nummen > 900] = NA
df$numwomen[df$numwomen > 900] = NA
df$partners[df$partners == 9] = NA
df$partnrs5[df$partnrs5 == 9] = NA
df$pistol[df$pistol == 3] = NA
df$polviews[df$polviews == 98] = NA
df$pres12[df$pres12 == 9] = NA
df$res2010[df$res2010 == 4] = NA
df$res2012[df$res2012 == 9] = NA
df$rifle[df$rifle == 3] = NA
df$shotgun[df$shotgun == 3] = NA






write.csv(df, "~/repos/signal/gss/gss.csv")
