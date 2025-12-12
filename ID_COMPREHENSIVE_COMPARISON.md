# Comprehensive Comparison: Indonesian (ID) vs English (EN/US) Time & TimeGrain

## Executive Summary

| Metric | EN | ID | Coverage |
|--------|----|----|----------|
| **Total Time Rules** | ~150+ | ~30 | **~20%** |
| **TimeGrain Rules** | 8 | 8 | **100%** ✅ |
| **Holiday Rules** | ~100+ | 23 | **~23%** |
| **Test Corpus Examples** | ~1900+ | ~80 | **~4%** |

## Detailed Rule-by-Rule Comparison

### 1. TimeGrain Rules ✅ COMPLETE

Both EN and ID have complete TimeGrain support:

| Grain | EN Pattern | ID Pattern | Status |
|-------|------------|------------|--------|
| Second | `sec(ond)?s?` | `detik?` | ✅ |
| Minute | `m(in(ute)?s?)?` | `menit?` | ✅ |
| Hour | `h(((ou)?rs?)\|r)?` | `jam?` | ✅ |
| Day | `days?` | `hari?` | ✅ |
| Week | `weeks?` | `minggu?` | ✅ |
| Month | `months?` | `bulan?` | ✅ |
| Quarter | `(quarter\|qtr)s?` | `(kuarter\|qtr)?` | ✅ |
| Year | `y(ea)?rs?` | `tahun?` | ✅ |

**Note:** ID TimeGrain is complete and functional.

---

### 2. Basic Date Formats ⚠️ PARTIAL

| Pattern | EN | ID | Status |
|---------|----|----|--------|
| dd/mm | ✅ | ✅ | ✅ |
| dd-mm | ✅ | ✅ | ✅ |
| dd.mm | ✅ | ✅ | ✅ |
| dd/mm/yyyy | ✅ | ✅ | ✅ |
| dd-mm-yyyy | ✅ | ✅ | ✅ |
| dd.mm.yyyy | ✅ | ✅ | ✅ |
| yyyy-mm-dd (ISO) | ✅ | ✅ | ✅ |
| mm/dd (US format) | ✅ | ❌ | ❌ Not needed for ID |
| mm/dd/yyyy (US) | ✅ | ❌ | ❌ Not needed for ID |
| dd month yyyy | ✅ | ✅ | ✅ |
| dd month | ✅ | ✅ | ✅ |
| month dd yyyy | ✅ | ❌ | ❌ Missing |
| month yyyy | ✅ | ❌ | ❌ Missing |
| yyyy (year only) | ✅ | ❌ | ❌ Missing |
| yyyy-mm (year-month) | ✅ | ❌ | ❌ Missing |

**ID Coverage: ~60%** - Missing month-first formats and year-only formats.

---

### 3. Days of Week ✅ COMPLETE

| Day | EN | ID | Status |
|-----|----|----|--------|
| Monday | ✅ | ✅ Senin | ✅ |
| Tuesday | ✅ | ✅ Selasa | ✅ |
| Wednesday | ✅ | ✅ Rabu | ✅ |
| Thursday | ✅ | ✅ Kamis | ✅ |
| Friday | ✅ | ✅ Jumat | ✅ |
| Saturday | ✅ | ✅ Sabtu | ✅ |
| Sunday | ✅ | ✅ Minggu/Ahad | ✅ |

**ID Coverage: 100%** ✅

---

### 4. Relative Time Expressions ❌ SEVERELY INCOMPLETE

| Expression | EN | ID | Status |
|------------|----|----|--------|
| now | ✅ | ✅ sekarang | ✅ |
| today | ✅ | ✅ hari ini | ✅ |
| tomorrow | ✅ | ✅ besok | ✅ |
| yesterday | ✅ | ✅ kemarin | ✅ |
| day after tomorrow | ✅ | ✅ lusa | ✅ |
| day before yesterday | ✅ | ✅ kemarin lusa | ✅ |
| **this week** | ✅ | ❌ | ❌ Missing |
| **this month** | ✅ | ❌ | ❌ Missing |
| **this year** | ✅ | ❌ | ❌ Missing |
| **next Monday** | ✅ | ❌ | ❌ Missing |
| **last Monday** | ✅ | ❌ | ❌ Missing |
| **this Monday** | ✅ | ❌ | ❌ Missing |
| **next week** | ✅ | ⚠️ minggu depan (partial) | ⚠️ |
| **last week** | ✅ | ⚠️ minggu lalu (partial) | ⚠️ |
| **next month** | ✅ | ⚠️ bulan depan (partial) | ⚠️ |
| **last month** | ✅ | ⚠️ bulan lalu (partial) | ⚠️ |
| **next year** | ✅ | ❌ | ❌ Missing |
| **last year** | ✅ | ❌ | ❌ Missing |
| **Monday before last** | ✅ | ❌ | ❌ Missing |
| **Monday after next** | ✅ | ❌ | ❌ Missing |

**ID Coverage: ~30%** - Missing most "this/next/last" combinations with days and years.

---

### 5. Intersect Rules ❌ COMPLETELY MISSING

These are **CRITICAL** for combining time expressions:

| Rule | EN | ID | Status |
|------|----|----|--------|
| `ruleIntersect` | ✅ | ❌ | ❌ **CRITICAL** |
| `ruleIntersectOf` | ✅ | ❌ | ❌ **CRITICAL** |
| `ruleIntersectYear` | ✅ | ❌ | ❌ **CRITICAL** |
| `ruleAbsorbOnDay` | ✅ | ❌ | ❌ Missing |
| `ruleAbsorbInMonthYear` | ✅ | ❌ | ❌ Missing |
| `ruleAbsorbCommaTOD` | ✅ | ❌ | ❌ Missing |

**Examples that won't work in ID:**
- ❌ "Senin jam 3" (Monday at 3pm)
- ❌ "15 Januari jam 2" (January 15 at 2pm)
- ❌ "Senin pertama bulan Januari" (first Monday of January)

**ID Coverage: 0%** - This is a **MAJOR GAP**.

---

### 6. Duration-Based Expressions ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "in 2 hours" | ✅ | ❌ | ❌ Missing |
| "2 hours ago" | ✅ | ❌ | ❌ Missing |
| "after 3 days" | ✅ | ❌ | ❌ Missing |
| "before 1 week" | ✅ | ❌ | ❌ Missing |
| "in 5 minutes" | ✅ | ❌ | ❌ Missing |
| "2 days from now" | ✅ | ❌ | ❌ Missing |
| "3 weeks ago" | ✅ | ❌ | ❌ Missing |
| "duration after time" | ✅ | ❌ | ❌ Missing |
| "duration before time" | ✅ | ❌ | ❌ Missing |

**Indonesian equivalents needed:**
- "dalam 2 jam" (in 2 hours)
- "2 jam yang lalu" (2 hours ago)
- "setelah 3 hari" (after 3 days)
- "sebelum 1 minggu" (before 1 week)

**ID Coverage: 0%** - **CRITICAL MISSING FEATURE**.

---

### 7. Ordinal Expressions ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "first Monday" | ✅ | ❌ | ❌ Missing |
| "second Tuesday" | ✅ | ❌ | ❌ Missing |
| "last Monday of month" | ✅ | ❌ | ❌ Missing |
| "15th day" | ✅ | ❌ | ❌ Missing |
| "first Monday of January" | ✅ | ❌ | ❌ Missing |
| "3rd week" | ✅ | ❌ | ❌ Missing |

**Indonesian equivalents needed:**
- "Senin pertama" (first Monday)
- "Selasa kedua" (second Tuesday)
- "Senin terakhir bulan Januari" (last Monday of January)
- "tanggal 15" (15th day)

**ID Coverage: 0%** - **CRITICAL MISSING FEATURE**.

---

### 8. Part of Day Expressions ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "morning" | ✅ | ❌ | ❌ Missing |
| "afternoon" | ✅ | ❌ | ❌ Missing |
| "evening" | ✅ | ❌ | ❌ Missing |
| "night" | ✅ | ❌ | ❌ Missing |
| "Monday morning" | ✅ | ❌ | ❌ Missing |
| "morning of Monday" | ✅ | ❌ | ❌ Missing |
| "after morning" | ✅ | ❌ | ❌ Missing |
| "noon" | ✅ | ❌ | ❌ Missing |
| "midnight" | ✅ | ❌ | ❌ Missing |
| "this morning" | ✅ | ❌ | ❌ Missing |
| "tonight" | ✅ | ❌ | ❌ Missing |

**Indonesian equivalents needed:**
- "pagi" (morning)
- "siang" (afternoon)
- "sore" (evening)
- "malam" (night)
- "Senin pagi" (Monday morning)
- "tengah hari" (noon)
- "tengah malam" (midnight)

**Note:** ID has basic "pagi/siang/sore/malam" in `ruleJamHHPartOfDay` but not as standalone expressions.

**ID Coverage: ~5%** - Only partial support in time expressions.

---

### 9. Time of Day Patterns ⚠️ PARTIAL

| Pattern | EN | ID | Status |
|---------|----|----|--------|
| HH:MM (24h) | ✅ | ✅ pukul 14:30 | ✅ |
| HH.MM (24h) | ✅ | ✅ pukul 14.30 | ✅ |
| HH (24h) | ✅ | ✅ jam 14 | ✅ |
| HH:MM:SS | ✅ | ❌ | ❌ Missing |
| HH:MM AM/PM | ✅ | ⚠️ jam 2 pagi/sore | ⚠️ Partial |
| "half past 2" | ✅ | ❌ | ❌ Missing |
| "quarter to 3" | ✅ | ❌ | ❌ Missing |
| "quarter past 3" | ✅ | ❌ | ❌ Missing |
| "2:30" | ✅ | ✅ | ✅ |
| "at 3pm" | ✅ | ⚠️ | ⚠️ Partial |
| "around 3pm" | ✅ | ❌ | ❌ Missing |
| "exactly 3pm" | ✅ | ❌ | ❌ Missing |

**ID Coverage: ~40%** - Basic time parsing works, but missing:
- Seconds (HH:MM:SS)
- Half/quarter expressions
- Precision modifiers (around, exactly)

---

### 10. Time Intervals ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "15-20 January" | ✅ | ❌ | ❌ Missing |
| "from 15 to 20 January" | ✅ | ❌ | ❌ Missing |
| "between 15 and 20" | ✅ | ❌ | ❌ Missing |
| "January 15-20" | ✅ | ❌ | ❌ Missing |
| "3pm-5pm" | ✅ | ❌ | ❌ Missing |
| "from 3pm to 5pm" | ✅ | ❌ | ❌ Missing |
| "for 2 hours" | ✅ | ❌ | ❌ Missing |
| "from 3pm for 2 hours" | ✅ | ❌ | ❌ Missing |

**Indonesian equivalents needed:**
- "15-20 Januari"
- "dari 15 sampai 20 Januari"
- "antara 15 dan 20"
- "jam 3 sampai jam 5"

**ID Coverage: 0%** - **CRITICAL MISSING FEATURE**.

---

### 11. Month/Year Expressions ⚠️ PARTIAL

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "January" | ✅ | ✅ Januari | ✅ |
| "January 2024" | ✅ | ❌ | ❌ Missing |
| "2024" | ✅ | ❌ | ❌ Missing |
| "January 15" | ✅ | ✅ 15 Januari | ✅ |
| "15 January" | ✅ | ✅ 15 Januari | ✅ |
| "January 15, 2024" | ✅ | ✅ 15 Januari 2024 | ✅ |
| "Q1 2024" | ✅ | ❌ | ❌ Missing |
| "2024 Q1" | ✅ | ❌ | ❌ Missing |

**ID Coverage: ~50%** - Missing month+year and year-only formats.

---

### 12. Cycle Expressions ❌ MOSTLY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "this week" | ✅ | ❌ | ❌ Missing |
| "next week" | ✅ | ⚠️ minggu depan | ⚠️ Partial |
| "last week" | ✅ | ⚠️ minggu lalu | ⚠️ Partial |
| "this month" | ✅ | ❌ | ❌ Missing |
| "next month" | ✅ | ⚠️ bulan depan | ⚠️ Partial |
| "last month" | ✅ | ⚠️ bulan lalu | ⚠️ Partial |
| "this year" | ✅ | ❌ | ❌ Missing |
| "next year" | ✅ | ❌ | ❌ Missing |
| "last year" | ✅ | ❌ | ❌ Missing |
| "week after next" | ✅ | ❌ | ❌ Missing |
| "month before last" | ✅ | ❌ | ❌ Missing |

**ID Coverage: ~20%** - Only basic "depan/lalu" for week/month, missing "this" and year cycles.

---

### 13. Time Precision/Approximation ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "around 3pm" | ✅ | ❌ | ❌ Missing |
| "approximately 3pm" | ✅ | ❌ | ❌ Missing |
| "exactly 3pm" | ✅ | ❌ | ❌ Missing |
| "about 3pm" | ✅ | ❌ | ❌ Missing |
| "3pm sharp" | ✅ | ❌ | ❌ Missing |

**Indonesian equivalents needed:**
- "sekitar jam 3" (around 3pm)
- "kira-kira jam 3" (approximately 3pm)
- "tepat jam 3" (exactly 3pm)

**ID Coverage: 0%**

---

### 14. End/Beginning of Period ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "end of month" | ✅ | ❌ | ❌ Missing |
| "beginning of month" | ✅ | ❌ | ❌ Missing |
| "end of year" | ✅ | ❌ | ❌ Missing |
| "beginning of year" | ✅ | ❌ | ❌ Missing |
| "end of week" | ✅ | ❌ | ❌ Missing |
| "beginning of week" | ✅ | ❌ | ❌ Missing |

**Indonesian equivalents needed:**
- "akhir bulan" (end of month)
- "awal bulan" (beginning of month)
- "akhir tahun" (end of year)
- "awal tahun" (beginning of year)

**ID Coverage: 0%**

---

### 15. Weekend/Weekday ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "weekend" | ✅ | ❌ | ❌ Missing |
| "weekday" | ✅ | ❌ | ❌ Missing |
| "this weekend" | ✅ | ❌ | ❌ Missing |
| "next weekend" | ✅ | ❌ | ❌ Missing |

**Indonesian equivalents needed:**
- "akhir pekan" (weekend)
- "hari kerja" (weekday)

**ID Coverage: 0%**

---

### 16. Season Expressions ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "spring" | ✅ | ❌ | ❌ Missing |
| "summer" | ✅ | ❌ | ❌ Missing |
| "fall/autumn" | ✅ | ❌ | ❌ Missing |
| "winter" | ✅ | ❌ | ❌ Missing |

**Note:** Indonesia has different seasons (dry/rainy), but this could still be implemented.

**ID Coverage: 0%**

---

### 17. Timezone Support ❌ COMPLETELY MISSING

| Expression | EN | ID | Status |
|------------|----|----|--------|
| "3pm EST" | ✅ | ❌ | ❌ Missing |
| "3pm WIB" | ❌ | ❌ | ❌ Missing |
| "3pm WITA" | ❌ | ❌ | ❌ Missing |
| "3pm WIT" | ❌ | ❌ | ❌ Missing |

**ID Coverage: 0%** - Would be useful for Indonesia's 3 timezones (WIB, WITA, WIT).

---

### 18. Holiday Rules ⚠️ PARTIAL

| Type | EN | ID | Status |
|------|----|----|--------|
| Fixed date holidays | ~80 | 23 | ⚠️ Partial |
| Computed holidays (Easter, etc.) | ~20 | 0 | ❌ Missing |
| Holiday intervals | ~5 | 0 | ❌ Missing |

**ID Coverage: ~23%** - Has basic fixed-date holidays, but missing:
- Computed holidays (Idul Fitri, Idul Adha - variable dates)
- Holiday intervals

---

## Summary by Category

| Category | EN Rules | ID Rules | Coverage | Priority |
|----------|----------|----------|----------|----------|
| **TimeGrain** | 8 | 8 | **100%** ✅ | ✅ Complete |
| **Basic Dates** | ~15 | 7 | **47%** | 🔴 High |
| **Days of Week** | 7 | 7 | **100%** ✅ | ✅ Complete |
| **Relative Time** | ~20 | 6 | **30%** | 🔴 High |
| **Intersect** | ~5 | 0 | **0%** | 🔴 **CRITICAL** |
| **Duration-Based** | ~10 | 0 | **0%** | 🔴 **CRITICAL** |
| **Ordinal** | ~8 | 0 | **0%** | 🔴 High |
| **Part of Day** | ~10 | 0 | **5%** | 🟡 Medium |
| **Time Patterns** | ~15 | 3 | **20%** | 🔴 High |
| **Intervals** | ~8 | 0 | **0%** | 🟡 Medium |
| **Month/Year** | ~8 | 4 | **50%** | 🔴 High |
| **Cycles** | ~10 | 4 | **40%** | 🔴 High |
| **Precision** | ~5 | 0 | **0%** | 🟢 Low |
| **End/Beginning** | ~6 | 0 | **0%** | 🟡 Medium |
| **Weekend/Weekday** | ~2 | 0 | **0%** | 🟡 Medium |
| **Seasons** | ~4 | 0 | **0%** | 🟢 Low |
| **Timezone** | ~4 | 0 | **0%** | 🟢 Low |
| **Holidays** | ~100 | 23 | **23%** | 🟡 Medium |
| **TOTAL** | **~150+** | **~30** | **~20%** | |

---

## Critical Missing Features (Must Have)

1. **Intersect Rules** - Cannot combine "Senin jam 3" (Monday at 3pm)
2. **Duration-Based** - Cannot parse "dalam 2 jam" (in 2 hours)
3. **"This" Expressions** - Cannot parse "minggu ini" (this week)
4. **Month/Year Only** - Cannot parse "Januari 2024" or "2024"
5. **Ordinal** - Cannot parse "Senin pertama" (first Monday)

## High Priority Missing Features

6. **Part of Day** - Cannot parse standalone "pagi", "siang", "sore", "malam"
7. **Time Intervals** - Cannot parse "15-20 Januari"
8. **Better Time Parsing** - Missing seconds, half/quarter expressions
9. **Year Cycles** - Cannot parse "tahun depan/lalu" (next/last year)

## Medium Priority Missing Features

10. **End/Beginning** - Cannot parse "akhir bulan" (end of month)
11. **Weekend** - Cannot parse "akhir pekan" (weekend)
12. **Time Precision** - Cannot parse "sekitar jam 3" (around 3pm)

## Low Priority Missing Features

13. **Seasons** - Not critical for Indonesia
14. **Timezone** - Nice to have for WIB/WITA/WIT
15. **More Holidays** - Can be added incrementally

---

## Recommendations

### Immediate Actions (Critical):
1. ✅ Fix regex syntax error - **DONE**
2. ✅ Add holidays - **DONE** (23 holidays)
3. ✅ Create corpus - **DONE**
4. **Implement intersect rules** - **NEXT PRIORITY**
5. **Implement duration-based expressions** - **NEXT PRIORITY**
6. **Add "this" time expressions** - **NEXT PRIORITY**

### Short-term (High Priority):
7. Add month/year without day formats
8. Add ordinal expressions
9. Add part of day expressions
10. Improve time parsing (seconds, half/quarter)

### Medium-term:
11. Add time intervals
12. Add end/beginning of period
13. Add weekend/weekday

### Long-term:
14. Add timezone support
15. Add computed holidays (Idul Fitri, Idul Adha)
16. Add more precision modifiers

---

## Test Coverage Comparison

| Language | Corpus Examples | Test Coverage |
|----------|----------------|---------------|
| EN | ~1900+ | Comprehensive |
| IT | ~867 | Good |
| ID | ~80 | **Minimal** ⚠️ |

**ID needs significantly more test cases** to validate all rules.

---

## Conclusion

Indonesian (ID) Time parsing has **~20% coverage** compared to English (EN). While TimeGrain is complete (100%), Time parsing is severely limited:

- ✅ **Complete:** TimeGrain, Days of Week
- ⚠️ **Partial:** Basic dates, Relative time (basic), Holidays
- ❌ **Missing:** Intersect, Duration-based, Ordinal, Part of day, Intervals, and many more

**The most critical gaps are:**
1. Intersect rules (cannot combine expressions)
2. Duration-based expressions (cannot parse "dalam 2 jam")
3. "This" time expressions (cannot parse "minggu ini")

These three features alone would significantly improve ID's usability.

