# GoDaddy
#  Female Entrepreneurs Cohort Analysis

This project explores the evolving landscape of women-led microbusinesses using GoDaddy’s survey data from 2023–2024. Conducted in partnership with Babson College, this data-driven R-based research aims to uncover stress patterns, entrepreneurial personas, and strategic opportunities for empowering women entrepreneurs.

**Tools Used:** R, ggplot2, dplyr, shiny, leaflet, Quarto

---

## A. Why This Project Matters

Since 2020, there has been a surge in women-led businesses, especially post-pandemic. However, this growth masks deeper challenges—financial strain, limited capital, time poverty, and underrepresentation in long-standing ventures.

By deeply analyzing three cohorts from GoDaddy's surveys (Feb '23, Aug '23, Feb '24), this project reveals how variables like business age, race, education, marital status, and startup capital intersect to shape entrepreneurial outcomes—offering actionable insights to improve equity and sustainability.

---

## B. Business Questions

- FROM SOLO TO TEAMWORK: STRESS PATTERNS IN WOMEN’S BUSINESSES: This bar chart shows how female entrepreneurs split between being solo and managing employees, based on how      many hours they work per week and their stress level (1–7).
![Screenshot 2025-06-13 132102](https://github.com/user-attachments/assets/17cba506-062f-41be-bd57-42f2039d4801)

- STRESS BY EDUCATION AND CAPITAL: The boxplot graph shows how startup capital levels relate to stress among female entrepreneurs, filtered by education level. 
![image](https://github.com/user-attachments/assets/71b81ae2-f1d4-42d7-b6bd-76057ce6ce65)

- INFLUENCE OF RACE AND MARITAL STATUS ON WORKING HOURS OF FEMALE ENTREPRENEURS: This heatmap shows the distribution of weekly hours worked by female entrepreneurs across       different marital statuses, and it updates based on the selected race category. 
![image](https://github.com/user-attachments/assets/fbfc7371-6dda-4d71-9d0d-e0febc5360af)

- INFLUENCE OF AGE ON WOMEN’S ENTREPRENURIAL PATTERNS: This graph displays the distribution of businesses by their age, segmented by the owner's age range.
![image](https://github.com/user-attachments/assets/74d62a0b-d9df-4b55-bede-bc1b15a8ca75)

---

## C. Project Objectives

- Segment women entrepreneurs by business age, revenue, and stress.
- Explore how race, marital status, and education affect business operations.
- Develop actionable personas based on business behavior and financial resilience.
- Recommend product and policy improvements for GoDaddy’s entrepreneurial users.

---

## D. Data & Methodology

### Data Sources

- **GoDaddy Microbusiness Surveys**  
  - February 2023  
  - August 2023  
  - February 2024  
- Includes ~10,000+ records across gender, capital, revenue, education, stress, and more.

### Data Preparation

- Merged and cleaned survey datasets
- Derived variables: business age, binary financial stress indicator
- Classified businesses: solo vs. with employees

### Methodology

- **EDA:** Bar plots, boxplots, heatmaps, and cohort visualizations
- **Segmentation:** CART-based personas using stress, hours, education, and capital
- **Interactive Dashboards:** R Shiny prototypes for dynamic filtering by race, marital status, and more

---

## E. Key Insights

### Gender Shift by Business Age  
- Women dominate newer ventures (2–3 years), but are underrepresented in 10+ year-old businesses.

### Top Stressor: Financial Pressure  
- Nearly **30%** of women cite finances as their main source of stress, followed by work-life balance.

### Work Hours Patterns  
- Most women report working only 1–10 hours/week. Patterns vary significantly by marital status and race.

---


## F. ML-Driven Persona Discovery

I used CART classification trees to define 8 clear entrepreneur personas:

### **Financially Stable (87%)**
- **The Full-Timer**: <55 years, <9-year-old biz, low capital, works 31–50+ hrs/week  
- **The Expert**: 55+, well-educated, <8 years in biz  
- **The Veteran**: 55–64, established biz, high resilience  
- **The Part-Timer**: <55, moderate capital, part-time work, side-hustlers  
- **The Starter**: Young, low education, very low capital  
- **The Senior**: 65+, long-established business

### **Financially Stressed (13%)**
- **The Struggler**: <55, low education, minimal capital, unstable hours  
- **The Hard Worker**: <55, low education, invested >$1,000, full-time, still stressed

---

## G. Strategic Recommendations

###  Financial Empowerment  
- Tiered pricing, micro-loans, and milestone-based discounts  
- AI-powered budgeting tools  

###  Digital Tools  
- Website templates for women-led sectors  
- Mobile-first commerce solutions  

###  Operational Efficiency  
- One-click automation, stress-aware UI, and service pause features  

###  Mentorship & Community  
- Industry-based mentorship matching  
- Dedicated forums and storytelling space  

###  Personalized Growth  
- Diagnostic tools, learning journeys, and live "office hours" with experts  

---

## H.  Technologies Used

| Layer      | Tools                             |
|------------|-----------------------------------|
| **Backend**        | R, dplyr, readr, tidyverse|
| **Visualization**  | ggplot2, leaflet          |
| **UI / Dashboard** | R Shiny, shinytheme       |
| **ML Models**      | CART Decision Trees       |
| **Docs**           | Quarto, GitHub            |

---

## I. Conclusion

Women are driving a new era of microbusiness—but they face structural barriers rooted in financial and time constraints. This project offers GoDaddy and other stakeholders the data and tools needed to tailor support, boost long-term business success, and foster inclusive innovation.
