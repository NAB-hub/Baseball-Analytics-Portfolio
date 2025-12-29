# Hitter Ranking System Analysis

## Current System Issues

### Problem 1: Walk Rate Overweighted
**Evidence:**
- Wes Clarke (Rank #4): 126 wRC+, **.224 ISO**, **17.0% BB** → 52.44/50 Performance
- Blaze Jordan (Rank #40): 110 wRC+, **.167 ISO**, **7.1% BB** → 40.34/50 Performance
- Austin Gauthier (Rank #1): 123 wRC+, **.123 ISO**, **17.0% BB** → 55.72/50 Performance

**Issue:** BB% gets 10% weight in Tier 1, but elite walk rates are inflating scores beyond actual offensive value. Gauthier has .123 ISO (weak power) but ranks #1 because of walks.

### Problem 2: Not Penalizing Recent Decline
**Evidence:**
- Wes Clarke 2025: .142 ISO at AAA, 6 HR in 465 PA
- System uses career ISO (.224) heavily weighted by 2023 breakout (26 HR, 2 years ago)
- No recency weight or "form" consideration

**Issue:** Players living off old hot streaks rank higher than current producers.

### Problem 3: Age Context Backwards for Rule 5
**Evidence:**
- Blaze Jordan: Age 22, AAA exposure → Younger = more upside
- Wes Clarke: Age 25, still at AA → Older = more concerning

**Issue:** For Rule 5, we want MLB-ready NOW, not 3-year development projects. System rewards youth too heavily.

### Problem 4: K% Not Penalized Enough
**Evidence:**
- Wes Clarke: 28.8% K rate (very high)
- Blaze Jordan: 12.8% K rate (excellent contact)
- System gives K% only 10% weight in Tier 1

**Issue:** High strikeout rates are massive red flags for MLB success. Should be weighted more.

### Problem 5: No Positional Value Adjustment
**Evidence:**
- All 1B/DH types ranked purely on hitting
- No bonus for SS/C/CF vs 1B-only

**Issue:** Rule 5 teams need roster flexibility. A 1B-only guy needs to hit 130 wRC+ to be worth a spot. A UTL/SS can contribute at 100 wRC+.

## What the System Does RIGHT

✅ **wRC+ as anchor metric** - Best all-in-one hitting stat
✅ **Upper-level experience matters** - AAA/AA exposure weighted properly
✅ **Sample size checks** - PA minimums are reasonable
✅ **Peak performance tracking** - Best season ISO/wRC+ shows ceiling

## Proposed Fixes

### Fix 1: Rebalance Tier 1 Weights
**Current:** wRC+ 35%, OBP 25%, ISO 20%, BB% 10%, K% 10%
**Proposed:** wRC+ 40%, ISO 25%, K% 20%, BB% 10%, OBP 5%

**Rationale:**
- wRC+ already includes walks, OBP is redundant
- ISO (power) is scarce, should be valued more
- K% is a better predictor than BB% for MLB success

### Fix 2: Add Recency Weight
**Proposed:** 
- Most recent season gets 50% weight
- Previous season gets 30% weight
- Career baseline gets 20% weight

**Rationale:** 2025 performance > 2023 breakout

### Fix 3: Adjust Age/Upside for Rule 5 Context
**Current:** Younger = better (age 22 > age 25)
**Proposed:** 
- Ages 23-26 = peak value (ready now + upside)
- Ages 27+ = slight penalty (older, blocked)
- Ages 21-22 = penalty (too raw, need development)

**Rationale:** Rule 5 draft = must stick on 26-man roster, not farm system

### Fix 4: Add Positional Value Multiplier
**Proposed Tiers:**
- Premium (C, SS, CF): 1.10x multiplier
- Middle (2B, 3B, OF): 1.05x multiplier  
- Corner (1B, LF, RF): 1.00x multiplier
- UTL (multi-position): 1.08x multiplier
- DH-only: 0.90x multiplier

**Rationale:** Defensive flexibility = roster value

### Fix 5: Flag "Blocked" Players
**Proposed:** Add warning for players in orgs with strong depth charts
- Example: Blaze Jordan (STL has Goldschmidt/Walker at 1B)
- Available = more attractive target

## Recommended Action

1. Create `51b_rank_hitters_v2.R` with revised weights
2. Re-run rankings with new system
3. Compare Wes Clarke vs Blaze Jordan rankings
4. Validate against gut-check (does Kala'i Rosario belong at #2?)
