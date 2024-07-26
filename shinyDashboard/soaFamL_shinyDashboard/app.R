#libraries and other sources
library(shiny)
library(shinydashboard)
library(visNetwork)
library(fontawesome)
library(shinyjs)
#header...long-term actuarial modeling...custom width
custom_width <- 325

header <- dashboardHeader(
  title = tagList( #dashboard main title
    icon("laptop-code",class = "pull-right"),
    "Long-Term Actuarial Modeling"
  ),
  titleWidth = custom_width #match custom width in sidebar
)
#sidebar...prob || product || profit...icon defined sidebar
sidebar <- dashboardSidebar(
  sidebarMenu( #sidebar menu
    menuItem('§1: Overview', icon = icon('clipboard-list',class = "pull-right"), tabName = 'overview'),
    menuItem("§2: Insurable Interest", icon = icon("user-injured",class = "pull-right"), tabName = "insurable"),
    menuItem("§3: Survival Models", icon = icon("heart-pulse",class = "pull-right"), tabName = "survival"),
    menuItem("§4: Life-Contingent Products", icon = icon("cart-shopping",class = "pull-right"), tabName = "product"),
    menuItem('§5: Policy Values', icon = icon('money-bill-trend-up',class = "pull-right"), tabName = 'profit'),
    menuItem('§6: Notes', icon = icon('note-sticky',class = "pull-right"), tabName = 'notes')
  ),
  width = custom_width #custom width matched with header
)
#body...tab defined content
body <- dashboardBody(
  tags$head(
    tags$style(HTML("
    .centered-row {
      display: flex;
      justify-content: center;
    }
  "))
  ),
  tags$head(
    tags$style(HTML("
    .centered-tabBox .description-content {
      padding: 10px;
      font-size: 14px;
      line-height: 1.5;
      overflow-y: auto;
      max-height: '50%';
      box-sizing: border-box;
    }"))
  ),
  tags$head( #css for centralizing tabBox
    tags$style(HTML("
        .centered-tabBox {
          display: flex;
          justify-content: center;
        }
      "))
  ),
    tags$head(
    tags$style(HTML("
      .box-content {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
      }
    "))
  ),
  tags$head( #css for creating white space
    tags$style(HTML("
        .white-space {
          height: 30px;  /* Adjust the height as needed */
        }
      "))
  ),
  tags$head( #css for creating white space
    tags$style(HTML("
        .white-space-mini {
          height: 10px;  /* Adjust the height as needed */
        }
      "))
  ),
  tabItems( #overview tab content
    tabItem(tabName = "overview",
            h2(
              class = "centralized-h2",
              tagList(
                "Overview",
                icon("clipboard-list",class = "pull-center")
              ),
              class = "heading-with-icon"
            ),
            h3(
              class = "left-h4",
              tagList(
                "Prelude",
                icon("section",class = "pull-left")
              ),
              class = "heading-with-icon"
            ),
            tags$style(HTML("
              .box-header .box-title {
                width: 100%;
                text-align: center;
              }
            ")),
            div(class = "white-space"),
            fluidRow( #prelude content
              class = "centered-tabBox",
              box( #overview title and 5 W's description
                title = tagList(shiny::icon("magnifying-glass-chart"), 'Welcome to the Long-term Fundamentals of Actuarial Mathematics dashboard!'),
                solidHeader = TRUE, status = "primary", width = 10,
                "Using the 5 W's framework to break down the goals and motivations behind the creation of the dashboard provides clarity, structure, comprehensiveness, accessibility for all users. By addressing each of the 5 W's, this introduction not only explains the what, why, how, who, and when of the Long-term Fundamentals of Actuarial Mathematics dashboard but also highlights the thoughtfulness and effort behind its creation. Applying this framework to our project for the 'Long-term Fundamentals of Actuarial Mathematics' dashboard helps clarify its purpose, scope, and benefits."
              )
            ),
            useShinyjs(),  # Initialize shinyjs
            fluidRow(
              tags$div(class = "custom-tabbox",
                 tags$div(class = "introNote",
                    actionButton("infoButton", "", icon = icon("comment-dots")))),
              hidden(div(id = "infoPanel", 
                         HTML("• In the context of explaining the Long-Term Fundamentals of Actuarial Mathematics dashboard using the 5 W's framework, <b>How</b> is used instead of <b>Where</b> because:"),
                         br(),
                         HTML("&emsp; - How addresses the methodology, and techniques used to develop and implement the dashboard; explains the steps, tools, and programming principles applied to create the dashboard; which is essential for understanding the project's execution and functionality."),
                         br(),
                         HTML("&emsp; - Where typically refers to the physical or virtual location where something occurs; in the context of a software tool like a dashboard; 'where' is less relevant because the dashboard is not tied to a specific location but rather to its usage and application context.")
                         ))),
            div(class = "white-space-mini"),
            fluidRow( # 5 W's explanation and break-down
              class = "centered-tabBox",
              tabBox(
                title = tagList(shiny::icon("clipboard-question"), "The 5 W's of this tool"),
                id = "abstract", height = "100px", side = 'left', width = '10',
                tabPanel("1. What is this tool?", 
                         "This tool is an examination, study, and learning assistant designed specifically for actuarial topics. It provides a comprehensive understanding of various actuarial subjects, offering an alternative to existing literature by integrating data science perspectives. The content is adapted from the latest Society of Actuaries (SOA) syllabus, ensuring it covers essential topics such as insurable interest, survival models, life-contingent products, policy value, and more."),
                tabPanel("2. Why is this tool relevant?", 
                         "This tool is relevant because it provides a unique learning approach by combining traditional actuarial science with modern data science techniques. This fusion allows for more dynamic and interactive learning experiences, which can enhance comprehension and retention of complex actuarial concepts. It also serves as a supplementary resource for actuarial students and educators, providing additional support and an alternative perspective to standard textbooks."),
                tabPanel("3. How is it implemented?", 
                         "The implementation of this tool is through Shiny dashboards, an R package that allows for the creation of interactive web applications. These dashboards provide an intuitive and user-friendly interface for exploring actuarial topics, making the learning process more engaging. The use of interactive elements like plots, sliders, and input fields allows users to visualize and manipulate data, fostering a deeper understanding of the material."),
                tabPanel("4. Who will benefit from it?", 
                         "The primary beneficiaries of this tool are actuarial students and educators. Students can use the dashboards to supplement their learning, gaining a better grasp of actuarial concepts through interactive and practical examples. Educators can utilize the tool to enhance their teaching methods, providing a more dynamic and engaging classroom experience. Additionally, data science enthusiasts interested in actuarial science will find this tool valuable as it bridges the gap between the two fields."),
                tabPanel("5. When can this tool be used?", 
                         "This tool can be used at any point in the actuarial learning journey, whether for initial learning, revision, or deepening understanding of specific topics. Its flexibility and adaptability make it a useful resource for ongoing education and professional development in actuarial and data science.")
              )),
            div(class = "white-space-mini"),
            h3(
              class = "left-h4",
              tagList(
                "Catalog", #catalog section for table of content in succeeding sections
                icon("section",class = "pull-left")
              ),
              class = "heading-with-icon"
            ),
            fluidRow(
              class = "centered-tabBox",
              box( #syllabus information
                title = tagList(shiny::icon("folder-open",class = "pull-right"), "SOA FAM Examination Study Information"),
                solidHeader = TRUE, status = "warning", width = 10,
                "The Society of Actuaries (SOA) offers various resources for Exam FAM preparation, which include regularly updated syllabi, past exam questions with solutions, practice question videos with solutions, study method advice, and tips for multiple choice exams. These resources can be accessed on the FAM study page on the SOA website.", br(), HTML("&emsp;- FAM study page link: www.soa.org/education/exam-req/edu-exam-fam/study/")
              )
            ),
            fluidRow( #catalog content
              class = "centered-tabBox",
              box( #insurable interest content
                title = tagList(shiny::icon("user-injured",class = "pull-right"), "Insurable Interest"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§7)", br(),
                "- Definition and application of concept" , br(),
                "- Identify long-term life contingent products"
              ),
              box( #survival models content
                title = tagList(shiny::icon("heart-pulse",class = "pull-right"), "Survival Models"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§8 & §9) ", br(),
                "- Parametric survival models" , br(),
                "- Life-tables" , br(),
                "- Force of Mortality" , br(),
                "- Moments of Curtate and Complete Future-Lifetime random variables", br(),
                "- Actuarial notations", br(),
                "- Life-table relationship with survival models", br(),
                "- Selection", br(),
                "- Parametric & Non-parametric Estimators"
              ),
              box( #products content
                title = tagList(shiny::icon("shopping-cart",class = "pull-right"), "Life-Contingent Products"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§10) ", br(),
                "- Present value valuation of Life-contingent products" , br(),
                "- Distribution properties of Life-contingent products" , br(),
                "- Fractional Age and Claim Acceleration approches" , br(),
                "- Relationships between various products", br(),
                "- Actuarial notations", br(),
                "- Impact of variations in underlying survival models"
              ),
              box( #policy values content
                title = tagList(shiny::icon("money-bill-trend-up",class = "pull-right"), "Policy Values"),
                solidHeader = TRUE, status = "warning", width = 3, height = '250px',
                "- SOA Exam FAM syllabus (§11) ", br(),
                "- Future Loss random variable for various products" , br(),
                "- Premium values based on Equivalence or Portfolio Percentile Principles" , br(),
                "- Net, Gross, and Modified-Net premium policies" , br(),
                "- Impact of changes in underlying survival models", br(),
                "- Extra Risk"
              )
            )
    ),
    tabItem(tabName = "insurable", #insurable interest content
            h2(
              class = "centralized-h2",
              tagList(
                "Insurable Interest",
                icon("user-injured",class = "pull-center")
              ),
              class = "heading-with-icon"
            ),
            tabsetPanel(
              tabPanel(
                "§1: background",
                div(class = "white-space"),
                h3(
                  class = "left-h4",
                  tagList(
                    "Brief History of Actuarial Science", #catalog section for table of content in succeeding sections
                    icon("section",class = "pull-left")
                  ),
                  class = "heading-with-icon"
                ),
                div(class = "white-space-mini"),
                fluidRow(
                  class = "centered-tabBox",
                  width = 12,
                  height = "300px",
                  visNetworkOutput("briefHistory_flowchart", height = "300px",width = '70%'),
                  column(
                    width = 8,
                    htmlOutput("briefHistory_nodeDescription"))),
                div(class = "white-space-mini"),
                h3(
                  class = "left-h4",
                  tagList(
                    "Concept of Insurable Interest", #catalog section for table of content in succeeding sections
                    icon("section",class = "pull-left")
                  ),
                  class = "heading-with-icon"
                ),
                fluidRow(
                  class = "centered-tabBox",
                  tabBox(
                    title = tagList(shiny::icon("clipboard-question"), "The 5 W's of Insurable Interest"),
                    id = "insurableIntro", height = "100px", side = 'left', width = 12,
                    tabPanel("1. What is insurable interest?", 
                             HTML("<b>Definition: </b>"),"Insurable interest is a legal and financial stake in the insured subject, whether it be a person, property, or event. It is a requirement for the issuance of an insurance policy and ensures that the policyholder will suffer a genuine loss or financial hardship if the insured event occurs.",
                             br(),
                             HTML("<b>Purpose: </b>"),"The purpose of insurable interest is to prevent gambling and ensure that insurance remains a mechanism for risk management rather than speculation."
                    ),
                    tabPanel("2. When is insurable interest relevant?", 
                             HTML("<b>Policy Inception: </b>"),"Insurable interest must exist at the time of policy inception for the policy to be valid. In life insurance, it typically must exist at the time of policy purchase.",
                             br(),
                             HTML("<b>Claim Time: </b>"),"For property and casualty insurance, insurable interest must often exist both at the time of policy inception and at the time of loss."
                    ),
                    tabPanel("3. Where does insurable interest apply?", 
                             HTML("<b>Life Insurance: </b>"),"Insurable interest is required between the policyholder and the insured, commonly in relationships such as family members, business partners, or creditors and debtors.",
                             br(),
                             HTML("<b>Property Insurance: </b>"),"Insurable interest applies to the policyholder's ownership or financial interest in property, such as a home, car, or business assets.",
                             br(),
                             HTML("<b>Health Insurance: </b>"),"Insurable interest is relevant in health policies where the policyholder typically has an insurable interest in their own health or that of family members."
                    ),
                    tabPanel("4. Why is insurable interest important?", 
                             HTML("<b>Legal Requirement: </b>"),"It is a legal requirement to establish that the policyholder has a legitimate interest in the insured subject. This prevents fraudulent activities and ensures that insurance serves its intended purpose of risk mitigation.",
                             br(),
                             HTML("<b>Moral Hazard Reduction: </b>"),"By requiring insurable interest, insurers reduce the risk of moral hazard, where the policyholder might otherwise have an incentive to cause a loss.",
                             br(),
                             HTML("<b>Economic Protection: </b>"),"Insurable interest protects the policyholder from financial loss, ensuring they have a real stake in the insured subject's safety or well-being."
                    ),
                    tabPanel("5. Who is involved in insurable interest?", 
                             HTML("<b>Policyholder: </b>"),"The individual or entity purchasing the insurance policy.",
                             br(),
                             HTML("<b>Insured: </b>"),"The person whose life or property is covered by the policy.",
                             br(),
                             HTML("<b>Insurance Company: </b>"),"The entity providing the insurance coverage.",
                             br(),
                             HTML("<b>Beneficiaries: </b>"),"Individuals or entities who will receive the insurance benefits."
                    )
                  )
                ),
                div(class = "white-space-mini"),
                useShinyjs(),  # Initialize shinyjs
                fluidRow(
                  tags$div(class = "custom-tabbox",tags$div(actionButton("infoButton", "", icon = icon("comment-dots")))),
                  hidden(div(id = "insurablePanel", 
                             HTML("• How is insurable interest determined?"),
                             br(),
                             HTML("&emsp; - <b> Life Insurance </b>: Insurable interest is generally determined by relationships of blood, marriage, or financial dependence. For example, spouses, parents, and business partners typically have an insurable interest in each other's lives."),
                             br(),
                             HTML("&emsp; - <b> Property Insurance </b>: Insurable interest is determined by ownership, possession, or direct financial involvement. For instance, homeowners have an insurable interest in their property, and businesses have an insurable interest in their assets."),
                             br(),
                             HTML("&emsp; - <b> Verification </b>: Insurers may require documentation or proof of the relationship or financial stake to verify insurable interest before issuing a policy. This may include birth certificates, marriage certificates, business contracts, or financial statements.")
                  )))
              ),
              tabPanel(
                "§2: long-term life contingent products",
                tabsetPanel(
                  tabPanel(
                    "§2.1: insurance contracts",
                    tabsetPanel(
                      tabPanel(
                        "§2.1.1: traditional insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Types of Traditional Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Traditional Insurance Contracts',
                            id = 'traditionalIntro', width = 10,
                            tabPanel("Whole-Life","Provides lifetime coverage with fixed premiums and a cash value component that accumulates over time. The cash value can be borrowed against or used to pay premiums."),
                            tabPanel("Term-Life","Offers coverage for a specified period (e.g., 10, 20, or 30 years). It is typically more affordable than whole life insurance and does not accumulate cash value."),
                            tabPanel("Endowment","Pays out a lump sum either on the insured's death or after a specified period. It combines elements of savings and protection.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "History of Traditional Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Traditional Insurance Contracts",
                            width = 10,status='warning',
                            "19th Century: These products were developed to meet the growing demand for financial protection and savings mechanisms. The establishment of life insurance companies and the use of life tables allowed for more accurate pricing and risk assessment."
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Evolution of Traditional Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Traditional Insurance Contracts",
                            width = 10,status='warning',
                            "Traditional insurance products, while providing essential protection and savings features, were often rigid in terms of premiums and benefits. The need for more flexible and customizable insurance solutions led to the development of modern insurance products."
                          ))
                      ),
                      tabPanel(
                        "§2.1.2: modern insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Types of Modern Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Modern Insurance Contracts',
                            id = 'modernIntro', width = 10,
                            tabPanel("Universal Life","Offers flexible premiums and death benefits, with a cash value component that earns interest based on market rates. Policyholders can adjust their premiums and death benefits to better suit their financial situation."),
                            tabPanel("Variable Life","Allows policyholders to invest the cash value in various investment options, such as stocks and bonds. This can potentially yield higher returns, though it also comes with higher risk."),
                            tabPanel("Indexed Universal Life","Ties the cash value growth to a stock market index, offering potential for higher returns with some level of protection against market downturns.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "History of Modern Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Modern Insurance Contracts",
                            width = 10, status='warning',
                            "Late 20th Century: The development of universal and variable life insurance products was driven by the need for more flexible financial solutions that could adapt to changing economic conditions and personal financial situations."
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Evolution to Modern Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Modern Insurance Contracts",
                            width = 10,status='warning',
                            "The transition from traditional to modern insurance products reflects a broader trend towards greater flexibility, investment options, and customization in financial services. Modern products are designed to better meet the diverse and changing needs of policyholders."
                          ))
                      ),
                      tabPanel(
                        "§2.1.3: long-term health insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Types of Long-Term Health Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Long-Term Health Insurance Contracts',
                            id = 'longTermIntro', width = 10,
                            tabPanel("Long-Term Care","Covers the cost of long-term care services, such as nursing home care, home health care, and assisted living. It addresses the financial risks associated with extended care needs."),
                            tabPanel("Disablility","Provides income replacement if the policyholder becomes unable to work due to a disability. It helps maintain financial stability during periods of disability.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "History of Long-Term Health Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Long-Term Health Insurance Contracts",
                            width = 10,status='warning',
                            "Late 20th Century: The growing recognition of the financial risks associated with aging and disability led to the development and expansion of long-term care and disability insurance products."
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Evolution from Traditional Long-Term Health Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          box(
                            title = "Long-Term Health Insurance Contracts",
                            width = 10,status='warning',
                            "Traditional health insurance often did not cover long-term care or provide adequate income replacement during periods of disability. The evolution towards specialized long-term care and disability insurance products reflects the need for more comprehensive coverage of health-related financial risks."
                          ))
                      ),
                      tabPanel(
                        "§2.1.4: mutual & proprietary insurance contracts",
                        div(class = "white-space"),
                        h2(
                          tagList(
                            "Mutual Insurance Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Mutual Insurance',
                            id = 'mutualIntro', width = 10,
                            tabPanel("Definition","Owned by policyholders, with profits distributed as dividends or used to reduce future premiums. Mutual insurers prioritize policyholder benefits."),
                            tabPanel("Types","Can include both traditional and modern life insurance products."),
                            tabPanel("Historical Context","Mutual insurance companies were among the earliest forms of insurance providers, emphasizing policyholder ownership and benefits.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Proprietary Insurance Contracts",
                            icon("section",class = "pull-left")
                          ),
                          class = "heading-with-icon"
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = 'Proprietary Insurance',
                            id = 'proprietaryIntro', width = 10,
                            tabPanel("Definition","Owned by shareholders, with profits distributed as dividends to shareholders. Proprietary insurers balance policyholder interests with shareholder returns."),
                            tabPanel("Types","Can include both traditional and modern life insurance products."),
                            tabPanel("Historical Context","Proprietary insurance companies emerged to attract investment capital and expand market offerings.")
                          )),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Relationship Between Mutual and Proprietary Contracts",
                            icon("section",class = "pull-left")
                          )),
                          div(class = "white-space-mini"),
                          fluidRow(
                            class = "centered-tabBox",
                            box(
                              title = "Mutual and Proprietary Contracts",
                              width = 10,status='warning',
                              "Both mutual and proprietary insurance companies offer similar products, but their ownership structures and profit distribution methods differ. Mutual insurers focus on policyholder benefits, while proprietary insurers aim to balance policyholder and shareholder interests."
                            ))
                      ),
                      tabPanel(
                        "§2.1.5: insurance contract underwriting",
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Underwriting Process",
                            icon("section",class = "pull-left")
                          )),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = tagList(shiny::icon("pen-nib"), "The 5 W's of Underwriting"),
                            id = "insurableIntro", height = "100px", side = 'left', width = 12,
                            tabPanel("1. What is underwriting?", 
                                     "Underwriting involves multiple stakeholders within an insurance company. The primary individuals involved are underwriters, who assess the risk of insuring applicants. Other key participants include medical professionals who provide health evaluations, actuarial teams who help in risk assessment and pricing, and insurance agents who gather initial applicant information."
                            ),
                            tabPanel("2. When is underwriting relevant?", 
                                     "Underwriting occurs after an insurance application is submitted but before the policy is issued. It is a critical step in the insurance process, ensuring that the insurance company can manage its risk effectively. The specific timeline can vary, but underwriting is typically done within a few weeks of receiving the application."
                            ),
                            tabPanel("3. Where does underwriting happen?", 
                                     "Underwriting is conducted within insurance companies, specifically within their underwriting departments. With the advent of technology, some aspects of underwriting can now be done remotely or through automated systems. Additionally, medical underwriting may involve clinics and laboratories where applicants undergo health evaluations and testing."
                            ),
                            tabPanel("4. Why is underwriting important?", 
                                     "The primary purpose of underwriting is to protect the insurance company from taking on excessive risk, which could lead to financial losses. By carefully evaluating each applicant, underwriters ensure that the premiums collected are adequate to cover potential claims. This process helps maintain the financial stability of the insurance company and allows it to offer competitive rates to its policyholders."
                            ),
                            tabPanel("5. Who is involved in underwriting?", 
                                     "Underwriting involves multiple stakeholders within an insurance company. The primary individuals involved are underwriters, who assess the risk of insuring applicants. Other key participants include medical professionals who provide health evaluations, actuarial teams who help in risk assessment and pricing, and insurance agents who gather initial applicant information."
                            )
                          )
                        ),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          tabBox(
                            title = tagList(shiny::icon("pen-nib"), "How Underwriting Works"),
                            id = "insurableIntro", height = "100px", side = 'left', width = 12,
                            tabPanel("1. Application Submission", 
                                     "The applicant submits a detailed application form, providing personal, medical, and financial information."
                            ),
                            tabPanel("2. Medical Underwriting", 
                                     HTML("<b>Medical History: </b>"),"Reviewing medical records and health questionnaires.",
                                     br(),
                                     HTML("<b>Examinations: </b>"),"Conducting physical exams, blood tests, and other medical evaluations as necessary.",
                                     br(),
                                     HTML("<b>Specialist Reports: </b>"),"Obtaining reports from medical specialists if required."
                            ),
                            tabPanel("3. Financial Underwriting", 
                                     HTML("<b>Income Verification: </b>"),"Assessing the applicant's income and financial stability.",
                                     br(),
                                     HTML("<b>Lifestyle Factors: </b>"),"Evaluating factors such as smoking, alcohol consumption, and hazardous activities."
                            ),
                            tabPanel("4. Risk Classification", 
                                     "Classifying the applicant into risk categories (e.g., preferred, standard, substandard) based on the underwriting assessment."
                            ),
                            tabPanel("5. Premium Calculation", 
                                     "Calculating the premium based on the risk classification, policy type, and coverage amount."
                            ),
                            tabPanel("6. Policy Issuance", 
                                     "Issuing the policy with the agreed terms, conditions, and premium rates."
                            )
                          )
                        ),
                        div(class = "white-space-mini"),
                        h2(
                          tagList(
                            "Brief History of Underwriting",
                            icon("section",class = "pull-left")
                          )),
                        div(class = "white-space-mini"),
                        fluidRow(
                          class = "centered-tabBox",
                          width = 12,
                          height = "300px",
                          visNetworkOutput("underwriting_flowchart", height = "300px",width = '70%'),
                          column(
                            width = 8,
                            htmlOutput("underwriting_nodeDescription", class = "description-content"))
                          )
                      )
                    )
                  ),
                  tabPanel(
                    "§2.2: annuity contracts",
                    h2(
                      tagList(
                        "Brief History of Annuity Contracts",
                        icon("section",class = "pull-left")
                      )),
                    div(class = "white-space-mini"),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 12,
                      height = "300px",
                      visNetworkOutput("annuityIntro_flowchart", height = "300px",width = '70%'),
                      column(
                        width = 8,
                        htmlOutput("annuityIntro_nodeDescription", class = "description-content"))
                    ),
                    div(class = "white-space-mini"),
                    h2(
                      tagList(
                        "Accrual Process",
                        icon("section",class = "pull-left")
                      )),
                    fluidRow(
                      width = 10,
                      class = "centered-tabBox",
                      box(
                        title = "Accural Process for Annuity Contracts",
                        "Annuities are typically acquired through a purchase process involving underwriting to assess the applicant's age, health status, and financial needs. The underwriting process ensures that the annuity is priced accurately based on the risk profile of the individual."
                      )
                    )
                  ),
                  tabPanel(
                    "§2.3: pension contracts",
                    h2(
                      tagList(
                        "Brief History of Pension Contracts",
                        icon("section",class = "pull-left")
                      )),
                    div(class = "white-space-mini"),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 12,
                      height = "300px",
                      visNetworkOutput("pensionIntro_flowchart", height = "300px",width = '70%'),
                      column(
                        width = 8,
                        htmlOutput("pensionIntro_nodeDescription", class = "description-content"))
                    ),
                    div(class = "white-space-mini"),
                    h2(
                      tagList(
                        "Accrual Process",
                        icon("section",class = "pull-left")
                      )),
                    fluidRow(
                      width = 10,
                      class = "centered-tabBox",
                      box(
                        title = "Accural Process for Pension Contracts",
                        "Pension benefits accrue over time based on contributions from both employers and employees. In defined benefit plans, benefits are typically calculated based on a formula considering years of service and salary. In defined contribution plans, benefits depend on the amount contributed and the investment performance of those contributions."
                      )
                    )
                  ),
                  tabPanel(
                    "§2.4: other contracts",
                    div(class = "white-space "),
                    h2(
                      tagList(
                        "Health and Disability Insurance",
                        icon("section",class = "pull-left")
                      )),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 10,
                      tabBox(
                        width = 10,
                        title = 'Health and Disability Insurance',
                        tabPanel("Brief History",
                                 tabBox(
                                   title = "Brief History of Health and Disability Insurance",
                                   tabPanel(
                                     "19th Century",
                                     "Health insurance emerged with mutual aid societies and early commercial policies. These policies were simple and often provided limited coverage for basic medical expenses."
                                   ),
                                   tabPanel(
                                     "20th Century",
                                     "The expansion of health and disability insurance included the growth of employer-sponsored health plans and the introduction of Medicare and Medicaid. These plans provided more comprehensive coverage and were typically acquired through employers or government programs."
                                   ),
                                   tabPanel(
                                     "21st Century",
                                     "The 21st century saw significant changes in health insurance, including the Affordable Care Act (ACA) in the United States. The ACA aimed to increase access to health insurance, improve coverage quality, and reduce costs for individuals and families."
                                   ))
                        ),
                        tabPanel("Accural Process",
                                 "Health and disability insurance policies are typically acquired through an application process involving underwriting to assess the applicant's health status and risk factors. This process ensures that premiums are appropriately priced based on the individual's health and potential for claims.")
                      )
                    ),
                    div(class = "white-space"),
                    h2(
                      tagList(
                        "Long-Term Care Insurance",
                        icon("section",class = "pull-left")
                      )),
                    div(class = "white-space-mini"),
                    fluidRow(
                      class = "centered-tabBox",
                      width = 10,
                      tabBox(
                        width = 10,
                        title = 'Long-Term Care Insurance',
                        tabPanel("Brief History",
                                 tabBox(
                                   title = "Brief History of Long-Term Care Insurance",
                                   tabPanel(
                                     "Late 20th Century",
                                     "Long-term care insurance developed to cover extended care services like nursing home and home health care. These policies were designed to address the financial risks of aging and long-term care needs."
                                   ),
                                   tabPanel(
                                     "21st Century",
                                     "The market for long-term care insurance has expanded, offering various coverage options and benefit structures. Policies may include features like inflation protection and hybrid products that combine long-term care coverage with life insurance or annuities."
                                   ))
                        ),
                        tabPanel("Accural Process",
                                 "Long-term care insurance is typically acquired through a detailed application process involving underwriting to assess the applicant's health and long-term care needs. This underwriting process ensures that the policy is priced based on the risk of the individual requiring long-term care services in the future.")
                      )
                    )
                  )
                )
              )
            )
    ),
    tabItem(tabName = "survival",
            fluidPage(
              tags$head(
                tags$style(HTML("
                                /* Centralize tabsetPanel tabs */
                                .nav-tabs {
                                    display: flex;
                                    justify-content: center;
                                }
                                .nav-tabs .nav-item {
                                    float: none;
                                }
                                /* Style for dashboardSidebar text */
                                .sidebar-menu li a {
                                    font-size: 16px;
                                    color: #ffffff;
                                }
                                .sidebar-menu li a:hover {
                                    background-color: #444444;
                                }
                                .centralized-h2 {
                                    text-align: center;
                                }
                                .left-h4 {
                                    text-align: left;
                                }
                                "))
              ),
              h2(
                class = "centralized-h2",
                tagList(
                  "Survival Models",
                  icon("heart-pulse",class = "pull-center")
                ),
                class = "heading-with-icon"
              ),
              tabsetPanel(
                tabPanel('§1: overview',
                         'probability distributions intro'
                ),
                tabPanel('§2: force of mortality',
                           sidebarPanel(
                             sliderInput("age", "life age (x)", 1, 130, 65)
                           )
                ),
                tabPanel('§3: curtate future life-time',
                         sidebarPanel(
                           sliderInput("age", "life age (x)", 1, 130, 65)
                         )
                ),
                tabPanel('§4: life tables and selection',
                         sidebarPanel(
                           sliderInput("age", "life age (x)", 1, 130, 65)
                         )
                ),
                tabPanel('§5: notes',
                         'probability distributions notes'
                )
              )
            )
    ),
    tabItem(tabName = "product",
            h2(
              class = "centralized-h2",
              tagList(
                "Life-Contingent Products",
                icon("cart-shopping",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    ),
    tabItem(tabName = "profit",
            h2(
              class = "centralized-h2",
              tagList(
                "Policy Values",
                icon("money-bill-trend-up",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    ),
    tabItem(tabName = "notes",
            h2(
              class = "centralized-h2",
              tagList(
                "Notes",
                icon("note-sticky",class = "pull-center")
              ),
              class = "heading-with-icon"
            )
    )
  )
)

#ui...header || sidebar || body...accumulate dashboard components
ui <- dashboardPage(
    header,
    sidebar,
    body
)

#server...input || output...accumulate server objects
server <- function(input, output) { 
  #----------------------------------------------------briefHistory flowchart----------------------------------------------------
  briefHistory_nodes <- data.frame(
    id = 1:7,
    label = c("Ancient Origins","17th Century","18th Century","19th Century","20th Century","21st Century","Today"),
    description = c("• <b>Early Beginnings: The Foundations of Actuarial Science</b> <br>
                    &emsp; - The origins of actuarial science can be traced back to ancient civilizations where early forms of risk management and insurance practices were developed. These practices included pooling resources to mitigate the effects of risks like natural disasters and trade losses. <br>
                    &emsp; - <b>Early Risk Management:</b> Communities and merchants pooled resources to share the burden of losses from natural disasters and trade. <br>
                    &emsp; - <b>Ancient Practices:</b> The Code of Hammurabi and Roman burial societies are early examples of risk management and mutual aid practices.",
                    
                    "• <b>Development of Probability Theory</b> <br>
                    &emsp; - The foundation of actuarial science was significantly influenced by the development of probability theory. Mathematicians like Blaise Pascal and Pierre de Fermat laid the groundwork for understanding risk and uncertainty. <br>
                    &emsp; - <b>Mathematical Foundations:</b> Probability theory allowed for the systematic quantification of risk, essential for developing insurance and financial products. <br>
                    &emsp; - <b>Influential Mathematicians:</b> Blaise Pascal and Pierre de Fermat were pioneers in probability theory, which is crucial for actuarial calculations.",
                    
                    "• <b>Establishment of Life Tables</b> <br>
                    &emsp; - Edmund Halley created the first life table in 1693, providing a systematic way to estimate the probability of death at various ages. This was a crucial development for life insurance. <br>
                    &emsp; - <b>Mortality Analysis:</b> Life tables enabled insurers to estimate life expectancy and set premiums accurately. <br>
                    &emsp; - <b>Halley's Contribution:</b> Edmund Halley's life table was a major milestone, forming the basis for modern actuarial science.",
                    
                    "• <b>Formation of Actuarial Societies & Advancements in Mathematics</b> <br>
                    &emsp; - The Institute of Actuaries was established in London in 1848, and the Actuarial Society of America was founded in 1889. These organizations played a critical role in formalizing actuarial education and professional standards. <br>
                    &emsp; - <b>Professionalization:</b> The formation of actuarial societies formalized the profession, setting standards for practice and education. <br>
                    &emsp; - <b>Mathematical Techniques:</b> Actuaries began using more sophisticated mathematical techniques to model and predict risks, including compound interest theory.",
                    
                    "• <b>Expansion of Actuarial Science & Regulatory Developments</b> <br>
                    &emsp; - The actuarial profession expanded beyond life insurance to include pensions, health insurance, and general insurance. The adoption of computers and software allowed for more complex modeling and data analysis. <br>
                    &emsp; - <b>Broader Applications:</b> Actuaries applied their skills to new areas such as pensions and health insurance. <br>
                    &emsp; - <b>Technological Integration:</b> Computers and software facilitated advanced modeling and data analysis. <br>
                    &emsp; - <b>Regulatory Frameworks:</b> Increased regulation ensured the protection of policyholders and the stability of financial systems.",
                    
                    "• <b>Integration of Technology, Broader Applications, & Professional Development</b> <br>
                    &emsp; - The use of big data, machine learning, and predictive analytics has revolutionized the actuarial profession. Actuaries now use these tools to analyze vast amounts of data, improve risk assessment, and develop new insurance products. <br>
                    &emsp; - <b>Technological Advancements:</b> Big data and machine learning have enhanced risk assessment and product development. <br>
                    &emsp; - <b>Broader Fields:</b> Actuarial science now includes enterprise risk management (ERM) and financial risk management. <br>
                    &emsp; - <b>Continuous Education:</b> Organizations like the Society of Actuaries (SOA) and the Casualty Actuarial Society (CAS) ensure ongoing professional development and high standards through rigorous exams and certification.",
                    
                    "• <b>Current State of the Profession</b> <br>
                    &emsp; - Actuaries are essential in various industries, providing expertise in risk assessment, financial modeling, and strategic planning. They continue to adapt to emerging risks such as cyber threats, climate change, and evolving regulatory landscapes. <br>
                    &emsp; - <b>Modern Challenges:</b> Actuaries address new risks like cyber threats and climate change. <br>
                    &emsp; - <b>Strategic Role:</b> Actuaries play a crucial role in strategic decision-making and advising businesses on risk management."),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f251", "f6f0", "f21a", "f238", "f072","f5e7","f253"),
    icon.size = 40,
    icon.color = c("red", "green", "brown", "purple", "orange","blue","black")
  )
  
  briefHistory_edges <- data.frame(from = c(1, 2, 3, 4, 5, 6), to = c(2, 3, 4, 5, 6, 7))
  
  output$briefHistory_flowchart <- renderVisNetwork({
    visNetwork(briefHistory_nodes, briefHistory_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 12, hierarchical = FALSE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$briefHistory_nodeDescription <- renderText({
    req(input$current_node_id)
    briefHistory_selectedNode <- briefHistory_nodes[briefHistory_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", briefHistory_selectedNode$label, br(), briefHistory_selectedNode$description)
  }) 
  #----------------------------------------------------popups...actionButtons----------------------------------------------------
  observeEvent(input$infoButton, {
    toggle("infoPanel")  # Toggle the visibility of the infoPanel
  })
  
  observeEvent(input$infoButton, {
    toggle("insurablePanel")  # Toggle the visibility of the infoPanel
  })
  #----------------------------------------------------underwriting flowchart----------------------------------------------------
  underwriting_nodes <- data.frame(
    id = 1:4,
    label = c("Early Underwriting",
              "Mid-20th Century",
              "Late 20th Century",
              "21st Century"),
    description = c("• <b> Early Underwriting: The Foundations of Actuarial Science </b> <br> 
                    &emsp; - In the early days, underwriting was a manual and labor-intensive process. Actuarial science was in its nascent stages, primarily focused on the development of mortality tables and basic statistical methods to assess risk. <br>
                    &emsp; - <b>Manual Processes and Personal Judgment:</b> Underwriters relied heavily on personal judgment and experience; thus decisions were made based on limited data, often derived from personal interviews, medical examinations, and historical records. <br>
                    &emsp; - <b>Early Actuarial Tools:</b> Actuaries used rudimentary mortality tables and simple statistical tools to assist in risk assessment; thus the accuracy of these methods was constrained by the limited data available and the manual nature of calculations.", 
                    "• <b> Mid-20th Century: The Rise of Statistical Methods and Early Technology </b> <br> 
                    &emsp; - The mid-20th century saw significant advancements in statistical methods and the initial integration of technology into underwriting processes. <br>
                    &emsp; - <b>Introduction of Statistical Methods:</b> Actuaries began to develop more sophisticated statistical models, incorporating factors such as age, gender, and occupation; thus improving the accuracy of risk assessments but still required significant manual effort.<br>
                    &emsp; - <b>Early Use of Technology:</b>The introduction of computers in the 1950s and 1960s allowed for more efficient data processing; and underwriting processes began to incorporate computerized systems to store and analyze data, albeit in a limited capacity.<br> 
                    &emsp; - <b>SOA Contributions:</b>The SOA played a pivotal role in advancing actuarial education and research, promoting the use of statistical methods and emerging technologies in the insurance industry.", 
                    "• <b> Late 20th Century: Digital Transformation and Predictive Analytics </b> <br> 
                    &emsp; - The late 20th century marked a transformative period for underwriting, driven by the digital revolution and the advent of predictive analytics. <br>
                    &emsp; - <b>Digital Transformation:</b> The widespread adoption of digital technologies revolutionized data collection and processing; allowing Insurers to implemente sophisticated software systems to automate underwriting processes, reducing the reliance on manual work.<br>
                    &emsp; - <b>Emergence of Predictive Analytics:</b>Predictive analytics became a cornerstone of modern underwriting, utilizing large datasets and advanced algorithms to forecast risk; resulting in Actuaries and Data Scientists collaborating to develop models that could predict future claims with greater precision.<br> 
                    &emsp; - <b>Integration of Medical Technology:</b>Advances in medical technology, such as electronic health records and genetic testing, provided underwriters with more comprehensive and accurate health data; thus allowing for more nuanced risk assessments based on an individual's medical history and genetic predispositions.", 
                    "• <b> 21st Century: Modern Underwriting and Continuous Innovation </b> <br> 
                    &emsp; - In the 21st century, underwriting has continued to evolve, embracing continuous innovation in data analytics, automation, and digital technology. <br>
                    &emsp; - <b>Advanced Data Analytics:</b> Insurers now leverage big data and machine learning algorithms to analyze vast amounts of information, from social media activity to real-time health data from wearable devices; enabling underwriters to assess risk with unprecedented accuracy and granularity.<br>
                    &emsp; - <b>Automation and Streamlined Processes:</b>Automated underwriting systems handle routine tasks, allowing underwriters to focus on complex cases that require human judgment, which streamlines the underwriting process, reducing turnaround times and operational costs.<br> 
                    &emsp; - <b>Comprehensive Databases:</b> Insurers maintain extensive databases that integrate data from various sources, including financial records, medical histories, and lifestyle information; to support more informed and holistic risk assessments.<br>
                    &emsp; - <b>Regulatory and Ethical Considerations:</b>The evolution of underwriting has also been shaped by regulatory and ethical considerations, particularly around data privacy and the use of genetic information; to ensure fair and ethical underwriting practices.<br> 
                    &emsp; - <b>SOA's Role in Ongoing Development:</b>The SOA continues to contribute to the field through research, professional development, and the dissemination of best practices; supporting actuaries in staying abreast of technological advancements and evolving regulatory landscapes."),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f251", "f6f0", "f21a", "f238"),
    icon.size = 40,
    icon.color = c("red", "green", "brown", "purple")
  )
  
  underwriting_edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
  
  output$underwriting_flowchart <- renderVisNetwork({
    visNetwork(underwriting_nodes, underwriting_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 64, hierarchical = TRUE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$underwriting_nodeDescription <- renderText({
    req(input$current_node_id)
    underwriting_selectedNode <- underwriting_nodes[underwriting_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", underwriting_selectedNode$label, br(), underwriting_selectedNode$description)
  }) 
  #----------------------------------------------------annuityIntro flowchart----------------------------------------------------
  annuityIntro_nodes <- data.frame(
    id = 1:4,
    label = c("Ancient Rome","17th Century","18th and 19th Centuries","20th Century to Present"),
    description = c("• <b>Ancient Rome</b> <br>
                    &emsp; - The origins of actuarial science can be traced back to ancient civilizations where early forms of risk management and insurance practices were developed. These practices included pooling resources to mitigate the effects of risks like natural disasters and trade losses.",
                    "• <b>17th Century</b> <br>
                    &emsp; - The first modern annuity contracts were sold by governments in Europe to raise funds. These contracts were more structured, often involving formal agreements between the government and individuals or institutions.",
                    "• <b>18th and 19th Centuries</b> <br>
                    &emsp; - Actuarial methods were developed to price annuities more accurately, using life expectancy tables and probabilistic models. Annuity contracts during this period were typically issued by insurance companies, with pricing based on rigorous actuarial calculations to ensure financial viability.",
                    "• <b>20th Century to Present</b> <br>
                    &emsp; - The annuity market has grown, with products such as variable annuities, indexed annuities, and immediate vs. deferred annuities. These contracts are typically acquired through insurance companies, with underwriting processes involving detailed financial and health assessments to determine the appropriate pricing and terms."
                    ),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f251", "f6f0", "f21a", "f253"),
    icon.size = 40,
    icon.color = c("red", "green", "brown", "black")
  )
  
  annuityIntro_edges <- data.frame(from = c(1, 2, 3), to = c(2, 3, 4))
  
  output$annuityIntro_flowchart <- renderVisNetwork({
    visNetwork(annuityIntro_nodes, annuityIntro_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 12, hierarchical = TRUE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$annuityIntro_nodeDescription <- renderText({
    req(input$current_node_id)
    annuityIntro_selectedNode <- annuityIntro_nodes[annuityIntro_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", annuityIntro_selectedNode$label, br(), annuityIntro_selectedNode$description)
  })
  #----------------------------------------------------pensionIntro flowchart----------------------------------------------------
  pensionIntro_nodes <- data.frame(
    id = 1:3,
    label = c("19th Century","20th Century","21st Century"),
    description = c("• <b>19th Century</b> <br>
                    &emsp; - Employer-sponsored pension plans began to emerge, with companies like American Express and the Pennsylvania Railroad offering pensions. These plans were typically unfunded, with benefits promised based on the employer's financial stability.",
                    "• <b>20th Century</b> <br>
                    &emsp; - The expansion of pension plans included the introduction of Social Security in 1935 and ERISA in 1974. These plans became more formalized, with employers and employees contributing to funded plans designed to provide retirement income based on years of service and salary history.",
                    "• <b>21st Century</b> <br>
                    &emsp; - The shift from defined benefit to defined contribution plans, such as 401(k) plans, has changed the accrual process. Defined contribution plans involve regular contributions from employers and employees into individual accounts, with the retirement benefit depending on the investment performance of these accounts."
    ),
    shape = "icon",
    icon.face = 'FontAwesome',
    icon.code = c("f251", "f6f0", "f253"),
    icon.size = 40,
    icon.color = c("red", "green", "black")
  )
  
  pensionIntro_edges <- data.frame(from = c(1, 2), to = c(2, 3))
  
  output$pensionIntro_flowchart <- renderVisNetwork({
    visNetwork(pensionIntro_nodes, pensionIntro_edges) %>%
      visNodes(shape = "icon", font = list(multi = TRUE)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(hover = TRUE) %>%  # Enable hover interaction to show tool tips
      visLayout(randomSeed = 12, hierarchical = TRUE) %>%  # Ensuring the layout is consistent
      visEvents(selectNode = "function(properties) {
        Shiny.onInputChange('current_node_id', properties.nodes[0]);
      }")
  })
  
  output$pensionIntro_nodeDescription <- renderText({
    req(input$current_node_id)
    pensionIntro_selectedNode <- pensionIntro_nodes[pensionIntro_nodes$id == input$current_node_id, ]
    paste0("<b> • Node: </b> <br> &emsp; - ", pensionIntro_selectedNode$label, br(), pensionIntro_selectedNode$description)
  })
}

#launch app
shinyApp(ui, server)