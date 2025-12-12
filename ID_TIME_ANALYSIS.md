# Indonesian (ID) Time Parsing - Missing Features Analysis

## Current Status

Indonesian (ID) has **basic** Time and TimeGrain support, but is missing many critical patterns that are available in well-supported languages like English (EN).

## Issues Found

### 1. **Regex Syntax Error** (Line 378)
```haskell
-- CURRENT (BROKEN):
[ regex "(pukul|pk|jam)\\s*(2[0-3]|[01]?\\d)[:\\.](?[0-5]\\d)" ]
--                                    ^^^ INVALID SYNTAX

-- SHOULD BE:
[ regex "(pukul|pk|jam)\\s*(2[0-3]|[01]?\\d)[:\\.]([0-5]\\d)" ]
```

### 2. **Empty Corpus File**
The `Duckling/Time/ID/Corpus.hs` file is empty, meaning there are no test cases to validate the rules.

## Missing Critical Patterns

### A. **Intersect Rules** (Combining Time Expressions)
**Missing:**
- `ruleIntersect` - Combine time expressions (e.g., "Monday at 3pm")
- `ruleIntersectOf` - "Monday of next week"
- `ruleIntersectYear` - "January of 2024"

**Example EN patterns:**
- "Monday at 3pm" → combines day of week + time of day
- "first Monday of January" → combines ordinal + day + month

### B. **Duration-Based Expressions**
**Missing:**
- `ruleDurationInWithinAfter` - "dalam 2 jam" (in 2 hours)
- `ruleDurationHenceAgo` - "2 jam yang lalu" (2 hours ago)
- `ruleDurationAfterBeforeTime` - "2 jam setelah jam 3" (2 hours after 3pm)
- `ruleInNumeral` - "dalam 5" (in 5 minutes, implicit)

**Indonesian equivalents needed:**
- "dalam X" / "dalam X jam/menit/hari" (in X)
- "X yang lalu" / "X lalu" (X ago)
- "setelah X" / "sebelum X" (after/before X)

### C. **Relative Time with Cycles**
**Missing:**
- `ruleNextTime` - "minggu depan" (next week) - **PARTIALLY EXISTS** but needs expansion
- `ruleLastTime` - "minggu lalu" (last week) - **PARTIALLY EXISTS** but needs expansion
- `ruleThisTime` - "minggu ini" (this week)
- `ruleNextDOW` - "Senin depan" (next Monday)
- `ruleLastDOW` - "Senin lalu" (last Monday)
- `ruleNthTime` - "minggu ke-3" (3rd week)

**Current ID only has:**
- `ruleMingguDepan` - "minggu depan"
- `ruleMingguLalu` - "minggu lalu"
- `ruleBulanDepan` - "bulan depan"
- `ruleBulanLalu` - "bulan lalu"

**Missing:**
- "tahun depan/lalu" (next/last year)
- "Senin depan/lalu" (next/last Monday)
- "minggu ini" (this week)
- "bulan ini" (this month)

### D. **Ordinal Expressions**
**Missing:**
- `ruleOrdinalDOWOfTime` - "Senin pertama bulan Januari" (first Monday of January)
- `ruleLastDOWOfTime` - "Senin terakhir bulan Januari" (last Monday of January)
- `ruleNthTimeOfTime` - "hari ke-3 bulan ini" (3rd day of this month)
- `ruleTheDOMOrdinal` - "tanggal 15" (15th day)

### E. **Part of Day Expressions**
**Missing:**
- `rulePartOfDays` - "pagi", "siang", "sore", "malam" (morning, afternoon, evening, night)
- `ruleAfterPartofday` - "setelah pagi" (after morning)
- `ruleTimePOD` - "Senin pagi" (Monday morning)
- `rulePODofTime` - "pagi hari Senin" (morning of Monday)
- `ruleNoonMidnightEOD` - "tengah hari", "tengah malam" (noon, midnight)

**Current ID only has:**
- Basic "pagi/siang/sore/malam" in `ruleJamHHPartOfDay` but not as standalone expressions

### F. **Month/Year Expressions**
**Missing:**
- `ruleMonthDOMNumeral` - "Januari 15" (January 15)
- `ruleDOMMonth` - "15 Januari" (15 January) - **PARTIALLY EXISTS** as `ruleDDMonth`
- `ruleDOMMonthYear` - "15 Januari 2024" (15 January 2024) - **EXISTS** as `ruleDDMonthYYYY`
- `ruleMonthYear` - "Januari 2024" (January 2024, without day)
- `ruleYear` - "2024" (year only)
- `ruleYearLatent` - Implicit year parsing

**Current ID has:**
- `ruleDDMonthYYYY` - "15 Januari 2024" ✓
- `ruleDDMonth` - "15 Januari" ✓
- Month parsing function `parseMonthID` ✓

**Missing:**
- "Januari 2024" (month + year without day)
- "2024" (year only)

### G. **Time Intervals**
**Missing:**
- `ruleIntervalDash` - "15-20 Januari" (January 15-20)
- `ruleIntervalFrom` - "dari 15 sampai 20 Januari" (from 15 to 20 January)
- `ruleIntervalBetween` - "antara 15 dan 20 Januari" (between 15 and 20 January)
- `ruleIntervalForDuration` - "dari jam 3 selama 2 jam" (from 3pm for 2 hours)

### H. **Time Precision/Approximation**
**Missing:**
- `ruleAboutTimeofday` - "sekitar jam 3" (around 3pm)
- `ruleTimeofdayApproximately` - "kira-kira jam 3" (approximately 3pm)
- `ruleTimeofdaySharp` - "tepat jam 3" (exactly 3pm)
- `ruleTODPrecision` - "jam 3 tepat" (3pm sharp)

### I. **Complex Time Combinations**
**Missing:**
- `ruleAbsorbOnDay` - "pada Senin" (on Monday)
- `ruleAbsorbInMonthYear` - "pada Januari" (in January)
- `ruleTimeBeforeLastAfterNext` - "Senin sebelum lalu" (Monday before last)
- `ruleLastWeekendOfMonth` - "akhir pekan terakhir bulan Januari"
- `ruleWeekend` - "akhir pekan" (weekend)

### J. **Time of Day Patterns**
**Missing:**
- `ruleTODAM` - "jam 3 pagi" (3am) - **PARTIALLY EXISTS** but needs improvement
- `ruleTODPM` - "jam 3 sore" (3pm) - **PARTIALLY EXISTS** but needs improvement
- `ruleHODHalf` - "setengah 3" (half past 2 / 2:30)
- `ruleHODQuarter` - "seperempat 3" (quarter past/to 3)
- `ruleHHMMSS` - "14:30:45" (hours:minutes:seconds)
- `ruleTODLatent` - Implicit time of day parsing

**Current ID has:**
- `rulePukulHHMM` - "pukul 14:30" ✓
- `ruleJamHH` - "jam 14" ✓
- `ruleJamHHPartOfDay` - "jam 2 pagi" ✓ (but logic may need improvement)

### K. **Seasonal Expressions**
**Missing:**
- `ruleSeason` - "musim panas" (summer), "musim hujan" (rainy season)

### L. **Timezone Support**
**Missing:**
- `ruleTimezone` - "jam 3 WIB" (3pm WIB timezone)

### M. **Holidays - UPDATED** ✅
**Now includes 23 holidays:**
- ✅ Tahun Baru Masehi (New Year)
- ✅ Hari Peristiwa Kapal Selam
- ✅ Hari Gizi Nasional
- ✅ Hari Perempuan Internasional
- ✅ Hari Musik Nasional
- ✅ Hari Kartini (Kartini Day)
- ✅ Hari Bumi (Earth Day)
- ✅ Hari Buruh Internasional (Labor Day)
- ✅ Hari Pendidikan Nasional (National Education Day)
- ✅ Hari Kebangkitan Nasional (National Awakening Day)
- ✅ Hari Lahir Pancasila (Pancasila Day)
- ✅ Hari Lingkungan Hidup Sedunia
- ✅ Hari Lahir Jakarta
- ✅ Hari Bhayangkara (Police Day)
- ✅ Hari Anak Nasional (National Children's Day)
- ✅ Hari Proklamasi Kemerdekaan (Independence Day) - with variations
- ✅ Hari Olahraga Nasional (National Sports Day)
- ✅ Hari Kesaktian Pancasila
- ✅ Hari Batik Nasional (National Batik Day)
- ✅ Hari Sumpah Pemuda (Youth Pledge Day)
- ✅ Hari Pahlawan (Heroes Day)
- ✅ Hari Guru Nasional (National Teachers Day)
- ✅ Hari Ibu (Mother's Day)
- ✅ Hari Raya Natal (Christmas)

**Note:** Islamic holidays (Idul Fitri, Idul Adha, etc.) are variable dates based on lunar calendar and would require computed holidays (like Easter in EN), which is more complex to implement.

## Comparison: ID vs EN Rule Count

| Category | EN Rules | ID Rules | Status |
|----------|----------|----------|--------|
| Basic dates | ~15 | 7 | ⚠️ Partial |
| Days of week | 7 | 7 | ✅ Complete |
| Relative time | ~20 | 6 | ❌ Missing many |
| Duration-based | ~10 | 0 | ❌ Missing |
| Intersect | ~5 | 0 | ❌ Missing |
| Ordinal | ~8 | 0 | ❌ Missing |
| Part of day | ~10 | 0 | ❌ Missing |
| Time intervals | ~8 | 0 | ❌ Missing |
| Time precision | ~5 | 0 | ❌ Missing |
| Holidays | ~20 | 23 | ✅ Good coverage |
| **TOTAL** | **~108** | **~43** | **~40% coverage** |

## Recommended Priority for Implementation

### High Priority (Critical for basic usage):
1. ✅ **Fix regex syntax error** in `rulePukulHHMM` - **COMPLETED**
2. **Add intersect rules** - Essential for combining expressions
3. **Add duration-based expressions** - "dalam 2 jam", "2 jam yang lalu"
4. **Add "this" time expressions** - "minggu ini", "bulan ini"
5. **Add month/year without day** - "Januari 2024", "2024"

### Medium Priority (Common use cases):
6. **Add part of day expressions** - "pagi", "siang", "sore", "malam"
7. **Add ordinal expressions** - "Senin pertama", "tanggal 15"
8. **Add time intervals** - "15-20 Januari"
9. **Improve time of day parsing** - Better AM/PM handling

### Low Priority (Nice to have):
10. **Add time precision** - "sekitar", "tepat"
11. **Add seasonal expressions**
12. **Add timezone support**
13. ✅ **Add more holiday rules** - **COMPLETED** (expanded from 9 to 23 holidays)

## Testing

The corpus file is **empty**, so there are no test cases. You should:
1. Create test cases in `Duckling/Time/ID/Corpus.hs`
2. Run tests: `stack test Duckling.Time.ID.Tests`
3. Verify all patterns work correctly

## Next Steps

1. ✅ Fix the regex syntax error immediately - **COMPLETED**
2. ✅ Expand holiday rules - **COMPLETED** (23 holidays now)
3. Implement high-priority missing patterns (intersect, duration-based, etc.)
4. Add comprehensive test corpus
5. Test with real-world Indonesian time expressions

## Recent Updates

- ✅ **Fixed regex syntax error** in `rulePukulHHMM` (line 378)
- ✅ **Expanded holidays** from 9 to 23 Indonesian national holidays and important dates

