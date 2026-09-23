
# 1. LOAD PACKAGES
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(skimr)
library(openxlsx)



# 2. IMPORT DATA
fmcg <- read_excel(
  "D:/r program/FMCG_B2B_Sales_Practice_5000.xlsx",
  sheet = "Orders"
)



# 3. UNDERSTAND DATA
dim(fmcg)
head(fmcg)
str(fmcg)

names(fmcg)

fmcg$Net_Sales
filter(fmcg, City == "Bengaluru")


names(fmcg)
summary(fmcg)



# 4. DATA CLEANING

colSums(is.na(fmcg))
is.na(fmcg)
colSums(is.na(fmcg))
fmcg %>%
  filter(is.na(Return_Reason)) %>%
  select(Order_ID, Return_Qty, Return_Reason) %>%
  head(10)

fmcg %>%
  filter(is.na(Lead_Source)) %>%
  select(Order_ID, Customer_Name, City, Lead_Source) %>%
  head(10)


is.na(fmcg$Lead_Source)
sum(is.na(fmcg$Lead_Source))
table(fmcg$Lead_Source)

is.na(fmcg$Lead_Source)

sum(is.na(fmcg$Lead_Source))
sum(duplicated(fmcg))


sum(duplicated(fmcg$Order_ID))

duplicated(fmcg$Order_ID)


sum(fmcg$Quantity <= 0)

sum(fmcg$Unit_Price <= 0)

sum(fmcg$Return_Qty > fmcg$Quantity)

sum(fmcg$Gross_Profit < 0)


fmcg %>%
  filter(Gross_Profit < 0) %>%
  select(
    Order_ID,
    Product,
    Unit_Price,
    Quantity,
    Discount_pct,
    Net_Sales,
    Cost_Price,
    COGS,
    Gross_Profit
  )
#Which products are causing these losses?
fmcg %>%
  filter(Gross_Profit < 0) %>%
  group_by(Product) %>%
  summarise(
    Loss_Orders = n(),
    Total_Loss = sum(Gross_Profit)
  ) %>%
  arrange(Total_Loss)




names(fmcg)


##whether discount is causing the losses
fmcg %>%
  filter(Gross_Profit < 0) %>%
  group_by(Discount_pct) %>%
  summarise(
    Loss_Orders = n(),
    Total_Loss = sum(Gross_Profit)
  ) %>%
  arrange(Total_Loss)

##calculate loss rate at every discount level

fmcg %>%
  group_by(Discount_pct) %>%
  summarise(
    Total_Orders = n(),
    Loss_Orders = sum(Gross_Profit < 0),
    Loss_Rate_pct = Loss_Orders / Total_Orders * 100,
    Average_Profit = mean(Gross_Profit),
    Total_Profit = sum(Gross_Profit)
  ) %>%
  arrange(Discount_pct)



##Which products are risky at 10% discount?

fmcg %>%
  filter(Discount_pct == 0.10) %>%
  group_by(Product) %>%
  summarise(
    Total_Orders = n(),
    Loss_Orders = sum(Gross_Profit < 0),
    Loss_Rate_pct = Loss_Orders / Total_Orders * 100,
    Average_Profit = mean(Gross_Profit),
    Total_Profit = sum(Gross_Profit)
  ) %>%
  filter(Loss_Orders > 0) %>%
  arrange(desc(Loss_Rate_pct))



#calculate the maximum safe discount

fmcg <- fmcg %>%
  mutate(
    Max_Discount_pct = (Unit_Price - Cost_Price) / Unit_Price
  )
head(fmcg)

#inspect the safe discount for the loss-making products
fmcg %>%
  filter(Gross_Profit < 0) %>%
  select(
    Product,
    Unit_Price,
    Cost_Price,
    Discount_pct,
    Max_Discount_pct
  ) %>%
  distinct()




#Filtering data

fmcg %>%
  filter(City == "Bengaluru")

#filter using two conditions
#Show only Bengaluru Restaurant customers.

fmcg %>%
  filter(
    City == "Bengaluru",
    Customer_Type == "Restaurant"
  )


##Next step: add a third condition
#Show only Bengaluru restaurant orders with Net Sales above ₹5,000.

fmcg %>%
  filter(
    City == "Bengaluru",
    Customer_Type == "Restaurant",
    Net_Sales > 5000
  )

#saving filtered data into a new object.

blr_restaurants_highsales <- fmcg %>%
  filter(
    City == "Bengaluru",
    Customer_Type == "Restaurant",
    Net_Sales > 5000
  )
dim(blr_restaurants_highsales)
View(blr_restaurants_highsales)




blr_restaurants_selected <- blr_restaurants_highsales %>%
  select(
    Order_ID,
    Order_Date,
    Customer_ID,
    Customer_Name,
    Salesperson,
    Product,
    Category,
    Net_Sales,
    Gross_Profit
  )
View(blr_restaurants_selected)
dim(blr_restaurants_selected)



#Which Bengaluru restaurant orders had the highest Net Sales?

blr_restaurants_selected %>%
  arrange(desc(Net_Sales))

#summarise() — turn many rows into key business numbers
blr_restaurants_selected %>%
  summarise(
    Total_Sales = sum(Net_Sales),
    Average_Sales = mean(Net_Sales),
    Total_Profit = sum(Gross_Profit),
    Average_Profit = mean(Gross_Profit),
    Number_of_Orders = n()
  )


#Which Bengaluru restaurant customer generated the highest sales?

blr_restaurants_selected %>%
  group_by(Customer_ID, Customer_Name) %>%
  summarise(
    Total_Sales = sum(Net_Sales),
    Total_Profit = sum(Gross_Profit),
    Orders = n()
  ) %>%
  arrange(desc(Total_Sales))



#Add Average Order Value and Gross Margin %

customer_summary <- blr_restaurants_selected %>%
  group_by(Customer_ID, Customer_Name) %>%
  summarise(
    Total_Sales = sum(Net_Sales),
    Total_Profit = sum(Gross_Profit),
    Orders = n(),
    Average_Order_Value = mean(Net_Sales),
    Gross_Margin_pct = Total_Profit / Total_Sales * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Sales))

customer_summary


sales_median <- median(customer_summary$Total_Sales)

margin_median <- median(customer_summary$Gross_Margin_pct)


customer_summary %>%
  filter(
    Total_Sales > sales_median,
    Gross_Margin_pct < margin_median
  ) %>%
  arrange(desc(Total_Sales))



#investigate whether discounts are responsible
customer_summary2 <- fmcg %>%
  filter(
    Customer_ID %in% c("C0003", "C0095", "C0217")
  ) %>%
  group_by(Customer_ID, Customer_Name) %>%
  summarise(
    Total_Sales = sum(Net_Sales),
    Total_Profit = sum(Gross_Profit),
    Average_Discount_pct = mean(Discount_pct) * 100,
    Maximum_Discount_pct = max(Discount_pct) * 100,
    Gross_Margin_pct = Total_Profit / Total_Sales * 100,
    Orders = n(),
    .groups = "drop"
  )

customer_summary2



#find which categories these customers buy

fmcg %>%
  filter(
    Customer_ID %in% c("C0003", "C0095", "C0217")
  ) %>%
  group_by(Customer_Name, Category) %>%
  summarise(
    Total_Sales = sum(Net_Sales),
    Total_Profit = sum(Gross_Profit),
    Orders = n(),
    Gross_Margin_pct = Total_Profit / Total_Sales * 100,
    .groups = "drop"
  ) %>%
  arrange(Customer_Name, desc(Total_Sales))



customer_category <- fmcg %>%
  filter(
    Customer_ID %in% c("C0003", "C0095", "C0217")
  ) %>%
  group_by(Customer_Name, Category) %>%
  summarise(
    Total_Sales = sum(Net_Sales),
    Total_Profit = sum(Gross_Profit),
    Orders = n(),
    Gross_Margin_pct = Total_Profit / Total_Sales * 100,
    .groups = "drop"
  ) %>%
  arrange(Customer_Name, desc(Total_Sales))
View(customer_category)




#Sales Contribution %
 ##percentage of each customer's sales comes from each category?
customer_category <- customer_category %>%
  group_by(Customer_Name) %>%
  mutate(
    Sales_Contribution_pct =
      Total_Sales / sum(Total_Sales) * 100
  ) %>%
  ungroup()
View(customer_category)



customer_category %>%
  arrange(
    Customer_Name,
    desc(Sales_Contribution_pct)
  )


customer_category <- customer_category %>%
  group_by(Customer_Name) %>%
  mutate(
    Contribution_Median = median(Sales_Contribution_pct),
    Margin_Median = median(Gross_Margin_pct),
    
    Category_Status = case_when(
      Sales_Contribution_pct >= Contribution_Median &
        Gross_Margin_pct >= Margin_Median ~ "High Sales + High Margin",
      
      Sales_Contribution_pct >= Contribution_Median &
        Gross_Margin_pct < Margin_Median ~ "High Sales + Low Margin",
      
      Sales_Contribution_pct < Contribution_Median &
        Gross_Margin_pct >= Margin_Median ~ "Low Sales + High Margin",
      
      TRUE ~ "Low Sales + Low Margin"
    )
  ) %>%
  ungroup()
View(customer_category)



customer_category %>%
  filter(Category_Status == "High Sales + Low Margin") %>%
  arrange(Customer_Name, desc(Sales_Contribution_pct))



priority_categories <- customer_category %>%
  filter(Category_Status == "High Sales + Low Margin") %>%
  arrange(Customer_Name, desc(Sales_Contribution_pct)) %>%
  select(
    Customer_Name,
    Category,
    Total_Sales,
    Total_Profit,
    Sales_Contribution_pct,
    Gross_Margin_pct,
    Category_Status
  )
priority_categories


p <- ggplot(
  priority_categories,
  aes(x = Category, y = Total_Sales)
) +
  geom_col() +
  coord_flip()
print(p)


#separate the customers
ggplot(
  priority_categories,
  aes(
    x = Category,
    y = Total_Sales,
    fill = Customer_Name
  )
) +
  geom_col(position = "dodge") +
  coord_flip()


#add title, axis labels, ₹ formatting, and clean theme
ggplot(
  priority_categories,
  aes(
    x = Category,
    y = Total_Sales,
    fill = Customer_Name
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₹",
      big.mark = ","
    )
  ) +
  labs(
    title = "High-Sales, Low-Margin Categories",
    subtitle = "Priority categories by key B2B customer",
    x = "Product Category",
    y = "Net Sales",
    fill = "Customer"
  ) +
  theme_minimal()




#Sort categories + put ₹ values on the bars

p2 <- ggplot(
  priority_categories,
  aes(
    x = Category,
    y = Total_Sales,
    fill = Customer_Name
  )
) +
  geom_col(position = "dodge") +
  coord_flip()
exists("p2")
p2


p3 <- ggplot(
  priority_categories,
  aes(
    x = reorder(Category, Total_Sales, FUN = sum),
    y = Total_Sales,
    fill = Customer_Name
  )
) +
  geom_col(position = "dodge") +
  coord_flip()

p3


#clean the labels and add ₹ formatting


p3 <- p3 +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₹",
      big.mark = ","
    )
  ) +
  labs(
    title = "High-Sales, Low-Margin Categories",
    subtitle = "Priority categories across key B2B customers",
    x = "Product Category",
    y = "Net Sales",
    fill = "Customer"
  ) +
  theme_minimal()

p3



#Show the actual ₹ value on each bar
p4 <- p3 +
  geom_text(
    aes(
      label = scales::label_number(
        prefix = "₹",
        big.mark = ",",
        accuracy = 1
      )(Total_Sales)
    ),
    position = position_dodge(width = 0.9),
    hjust = -0.1,
    size = 3
  ) +
  expand_limits(
    y = max(priority_categories$Total_Sales) * 1.18
  )

p4


# Sales Contribution vs Gross Margin


p5 <- ggplot(
  customer_category,
  aes(
    x = Sales_Contribution_pct,
    y = Gross_Margin_pct,
    color = Customer_Name,
    size = Total_Sales
  )
) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%")
  ) +
  labs(
    title = "Sales Contribution vs Gross Margin",
    subtitle = "Customer-category profitability analysis",
    x = "Sales Contribution",
    y = "Gross Margin",
    color = "Customer",
    size = "Net Sales"
  ) +
  theme_minimal()

p5


#Add median reference lines

x_median <- median(customer_category$Sales_Contribution_pct)

y_median <- median(customer_category$Gross_Margin_pct)


p6 <- p5 +
  geom_vline(
    xintercept = x_median,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = y_median,
    linetype = "dashed"
  )

p6


#label the four quadrants

p7 <- p6 +
  annotate(
    "text",
    x = 2,
    y = 21,
    label = "Low Sales\nHigh Margin",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 23,
    y = 21,
    label = "High Sales\nHigh Margin",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 2,
    y = 3,
    label = "Low Sales\nLow Margin",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 23,
    y = 3,
    label = "High Sales\nLow Margin",
    size = 3.5
  )

p7



#label only the risky points

risky_points <- customer_category %>%
  filter(
    Sales_Contribution_pct > x_median,
    Gross_Margin_pct < y_median
  )
risky_points
p8 <- p7 +
  geom_text(
    data = risky_points,
    aes(
      label = paste(Customer_Name, Category, sep = " - ")
    ),
    hjust = -0.1,
    vjust = -0.4,
    size = 3,
    show.legend = FALSE
  )

p8


#Date analysis

fmcg <- fmcg %>%
  mutate(
    Year = year(Order_Date),
    Month = month(Order_Date, label = TRUE),
    Year_Month = floor_date(Order_Date, "month"),
    Quarter = quarter(Order_Date),
    Weekday = wday(Order_Date, label = TRUE)
  )

fmcg %>%
  select(
    Order_Date,
    Year,
    Month,
    Year_Month,
    Quarter,
    Weekday
  ) %>%
  head(10)



monthly_sales <- fmcg %>%
  group_by(Year_Month) %>%
  summarise(
    Total_Sales = sum(Net_Sales),
    Total_Profit = sum(Gross_Profit),
    Orders = n(),
    .groups = "drop"
  ) %>%
  arrange(Year_Month)

monthly_sales


#How much did sales increase or decrease compared with the previous month?
monthly_sales <- monthly_sales %>%
  mutate(
    Previous_Month_Sales = lag(Total_Sales),
    MoM_Growth_pct =
      (Total_Sales - Previous_Month_Sales) /
      Previous_Month_Sales * 100
  )
monthly_sales



p_monthly <- ggplot(
  monthly_sales,
  aes(x = Year_Month, y = Total_Sales)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₹",
      scale = 0.001,
      suffix = "K"
    )
  ) +
  labs(
    title = "Monthly FMCG B2B Sales Trend",
    subtitle = "Net sales performance over time",
    x = "Month",
    y = "Net Sales"
  ) +
  theme_minimal()

p_monthly


monthly_sales_complete <- monthly_sales %>%
  filter(
    Year_Month < floor_date(max(fmcg$Order_Date), "month")
  )
p_monthly_complete <- ggplot(
  monthly_sales_complete,
  aes(x = Year_Month, y = Total_Sales)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    labels = scales::label_number(
      prefix = "₹",
      scale = 0.001,
      suffix = "K"
    )
  ) +
  labs(
    title = "Monthly FMCG B2B Sales Trend",
    subtitle = "Completed months only",
    x = "Month",
    y = "Net Sales"
  ) +
  theme_minimal()

p_monthly_complete
