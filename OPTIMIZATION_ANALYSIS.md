# Indonesian Time Parsing - Optimization Analysis

## Summary
After reviewing `Duckling/Time/ID/Rules.hs` and `Duckling/TimeGrain/ID/Rules.hs`, here are the optimization opportunities found:

---

## 1. ✅ **Duplicate Language Pragmas** (Lines 7-18)
**Issue**: The language pragmas are declared twice at the top of `Time/ID/Rules.hs`

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ID.Rules
  ( rules
  ) where

{-# LANGUAGE GADTs #-}              -- DUPLICATE
{-# LANGUAGE NoRebindableSyntax #-} -- DUPLICATE
{-# LANGUAGE OverloadedStrings #-}  -- DUPLICATE
```

**Fix**: Remove lines 16-18 (duplicate pragmas after module declaration)

---

## 2. 🔄 **Redundant Week-Related Rules**
**Issue**: `ruleMingguKemaren` and `ruleMingguKemarin` do exactly the same thing

```haskell
ruleMingguKemaren :: Rule     -- "kemaren" (informal)
ruleMingguKemarin :: Rule     -- "kemarin" (formal)
```

**Options**:
- **Keep both**: Good for user flexibility (informal vs formal)
- **Combine regex**: `regex "kemaren|kemarin"` in one rule ✅ (RECOMMENDED)

---

## 3. 🔄 **Redundant Regex Patterns**

### 3a. `ruleBulanIniSampaiSekarang` (line 547)
```haskell
regex "sampai|hingga|sampai dengan"
```
**Issue**: "sampai dengan" contains "sampai", so it will never match
**Fix**: Use `"sampai( dengan)?|hingga"`

### 3b. `ruleDurationYangLalu` (line 621)
```haskell
regex "yang lalu|lalu|yang sudah lewat"
```
**Issue**: "lalu" is already in "yang lalu", may cause unexpected priority
**Fix**: Better order: `"yang (lalu|sudah lewat)|lalu"`

### 3c. Similar patterns for "sampai|sampe|hingga"
Used in multiple rules. Could be a named constant for consistency.

---

## 4. ⚡ **parseMonthID Performance**
**Current**: Uses case statement with 20+ conditions (O(n) lookup)

```haskell
parseMonthID :: Text.Text -> Maybe Int
parseMonthID raw =
  case Text.toLower raw of
    "januari" -> Just 1
    "jan"     -> Just 1
    ...
```

**Optimization**: Use HashMap for O(1) lookup

```haskell
import qualified Data.HashMap.Strict as HashMap

monthIDMap :: HashMap Text Int
monthIDMap = HashMap.fromList
  [ ("januari", 1), ("jan", 1)
  , ("februari", 2), ("feb", 2)
  ...
  ]

parseMonthID :: Text.Text -> Maybe Int
parseMonthID = flip HashMap.lookup monthIDMap . Text.toLower
```

**Impact**: Minimal for typical usage, but better practice

---

## 5. 🔄 **Consolidate Similar Date Format Rules**

**Current**: 6 separate rules for DD/MM, DD-MM, DD.MM formats (with and without year)

```haskell
ruleDDMM        -- dd/mm
ruleDDMMDash    -- dd-mm
ruleDDMMDot     -- dd.mm
ruleDDMMYYYY     -- dd/mm/yyyy
ruleDDMMYYYYDash -- dd-mm-yyyy
ruleDDMMYYYYDot  -- dd.mm.yyyy
```

**Consolidation Option** (Advanced):
```haskell
ruleDDMMSeparated :: Rule
ruleDDMMSeparated = Rule
  { name = "dd/mm or dd-mm or dd.mm"
  , pattern = [ regex "(3[01]|[12]\\d|0?[1-9])\\s*[/\\-.]\\s*(1[0-2]|0?[1-9])(?:\\s*[/\\-.]\\s*(\\d{2,4}))?" ]
  ...
```

**Recommendation**: Keep separate for clarity and debugging ✅

---

## 6. 🔄 **Month-to-Date Rules Can Be Simplified**

**Current**: Three very similar rules
- `ruleBulanIniSampaiSekarang` (lines 541-553)
- `ruleDariAwalBulanSampaiSekarang` (lines 556-569)
- `ruleSejakAwalBulan` (lines 572-583)

All produce the same result: interval from start of month to now

**Consolidation Option**:
```haskell
ruleMonthToDate :: Rule
ruleMonthToDate = Rule
  { name = "month to date"
  , pattern =
    [ regex "(bulan ini|dari awal bulan|sejak awal bulan|sejak permulaan bulan)"
    , regex "(sampai|hingga)?"
    , regex "(sekarang|hari ini)?"
    ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Closed start now
  }
```

**Trade-off**: Less explicit pattern matching but more maintainable

---

## 7. ✅ **Good Practices Already Implemented**

### Rule Ordering
- ✅ `ruleNumeralMingguLalu` comes before `ruleMingguLalu` (correct precedence)
- ✅ Specific rules before general rules

### TimeGrain Negative Lookahead
- ✅ `"minggu(?!\\s+(lalu|depan|kemaren|kemarin|kemudian))"` prevents conflicts

### Timezone Configuration
- ✅ Properly set to Asia/Jakarta in Dockerfile and ExampleMain.hs

---

## 8. 📝 **Corpus Coverage**

**Current corpus has**: 236 lines of test examples
**Coverage**:
- ✅ Basic dates and times
- ✅ Relative expressions  
- ✅ Holidays
- ❌ Missing: Intervals ("dari X sampai Y")
- ❌ Missing: Part of day combinations ("besok pagi", "tadi malam")
- ❌ Missing: Month-to-date expressions
- ❌ Missing: "N minggu lalu" patterns

**Recommendation**: Add test cases for recently added rules

---

## Priority Recommendations

### High Priority (Do Now)
1. ✅ **Remove duplicate language pragmas** (lines 16-18)
2. ✅ **Combine `ruleMingguKemaren` and `ruleMingguKemarin`** into one rule

### Medium Priority (Consider)
3. 🔄 **Fix redundant regex patterns** (sampai dengan, yang lalu)
4. 🔄 **Add corpus test cases** for new functionality

### Low Priority (Nice to Have)
5. ⚡ **Optimize `parseMonthID` with HashMap**
6. 📖 **Add inline documentation** for complex rules

---

## Performance Characteristics

**Current Implementation**:
- Rule count: 52 rules (including 23 holidays)
- Average pattern complexity: Low to Medium
- Regex efficiency: Good (most patterns are simple)

**Bottlenecks**: None identified for typical usage

**Memory footprint**: Acceptable (minimal state, mostly pure functions)

---

## Conclusion

The code is **well-structured and functional**. The main issues are:
1. Minor code cleanliness (duplicate pragmas)
2. Slight redundancy (similar rules could be combined)
3. Missing test coverage for new features

**Overall Grade**: A- (Very Good)

The optimizations are mostly cosmetic. The current implementation works well and is maintainable.

