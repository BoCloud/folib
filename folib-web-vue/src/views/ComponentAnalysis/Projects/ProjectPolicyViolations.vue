<template>
    <div class="wrapper">
        <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
            <div class="mx-25 search">
                <a-col :span="24" class="text-right">
                    <a-input-search
                        :placeholder="$t('Projects.NameQuery')"
                        class="v-search"
                        v-model="queryParams.searchText"
                        @search="handheTableSearch()"
                    />
                </a-col>
            </div>
            <a-table
                rowKey="uuid"
                class="mt-20"
                :columns="i18nColumns"
                :data-source="projectsData"
                :loading="loading"
                @change="handleChangeTable"
                @expanded="handleExpand"
                @expandedRowsChange="onExpandedRowsChange"
                :expanded-row-keys="expandedRowKeys"
                :pagination="pagination"
            >
                <template slot="name" slot-scope="name, row">
                    <a-button type="link" @click="handleGoDetail(row)">
                        {{ name }} {{ row.component.version }}
                    </a-button>
                </template>

                <template slot="timestamp" slot-scope="timestamp">
                    {{ typeof timestamp !== "undefined" ? formatTimestamp(timestamp, true,$t('Projects.FormatTime')) : "-" }}
                </template>

                <template slot="analysis" slot-scope="analysis">
                    {{ analysis ? $t(`Projects.ANALYSIS_${analysis}`) : $t(`Projects.ANALYSIS_NOT_SET`) }}
                </template>
                <template slot="suppressed" slot-scope="suppressed">
                    {{ suppressed ? $t(`Projects.Suppressed_${suppressed}`) : $t(`Projects.Suppressed_false`) }}
                </template>
                <span slot="expandIcon" slot-scope="props">
                    <img
                        src="@/assets/img/pull-down.svg"
                        alt="pull-down"
                        :class="{'is-expand': props.expanded}"
                        @click="props.onExpand"
                    >
                </span>
              <div slot="expandedRowRender" slot-scope="record">
                <policy-expanded-content  ref="PolicyExpanded" :record="record"></policy-expanded-content>
              </div>
<!--                <div slot="expandedRowRender" slot-scope="record" style="margin: 0">-->
<!--                    <div class="by-flex by-row-between by-col-top">-->
<!--                        <div class="disabled-textarea by-m-r-20">-->
<!--                            <div class="textarea-label">{{ $t('Projects.Condition') }}</div>-->
<!--                            <a-input-->
<!--                                :defaultValue="getConditionVal(record.policyCondition)"-->
<!--                                type="textarea"-->
<!--                                disabled-->
<!--                                :rows="6"-->
<!--                            ></a-input>-->
<!--                        </div>-->
<!--                        <div class="disabled-textarea">-->
<!--                            <div class="textarea-label">{{ $t('Projects.AuditTrail') }}</div>-->
<!--                            <a-input-->
<!--                                :defaultValue="getAuditVal(record)"-->
<!--                                v-model="commentsData"-->
<!--                                type="textarea"-->
<!--                                disabled-->
<!--                                :rows="6"-->
<!--                            ></a-input>-->
<!--                        </div>-->
<!--                    </div>-->
<!--                    <div class="commit by-m-t-20">-->
<!--                        <div class="commit-label">{{ $t('Projects.Commit') }}</div>-->
<!--                        <a-input-->
<!--                            v-model="commit"-->
<!--                            :placeholder="$t('Projects.CommitPlaceholder')"-->
<!--                            type="textarea"-->
<!--                            :rows="6"-->
<!--                            class="by-m-t-10 by-m-b-10"-->
<!--                            @change="handleChange"-->
<!--                        ></a-input>-->
<!--                        <a-button class="add-commit" @click="addCommit(record)">{{ $t('Projects.AddCommit') }}</a-button>-->
<!--                    </div>-->
<!--                    <div class="by-flex by-m-t-20">-->
<!--                        <div class="by-flex">-->
<!--                            <span class="by-m-r-10">{{ $t('Projects.Analysis') }}</span>-->
<!--                            <a-select-->
<!--                                v-model="analysisState"-->
<!--                                class="analysis"-->
<!--                                @change="handleAnalysisChange"-->
<!--                            >-->
<!--                                <a-select-option-->
<!--                                    v-for="(item, i) in analysisOptions"-->
<!--                                    :value="item.value"-->
<!--                                    :key="i"-->
<!--                                >-->
<!--                                    {{ $t(`Projects.${item.label}`) }}-->
<!--                                </a-select-option>-->
<!--                            </a-select>-->
<!--                        </div>-->
<!--                        <div class="by-m-l-45 by-flex">-->
<!--                            <span class="by-m-r-10">{{ $t('Projects.Suppressed') }}</span>-->
<!--                            <a-switch-->
<!--                                v-model="isSuppressed"-->
<!--                                checked-children="是"-->
<!--                                un-checked-children="否"-->
<!--                                @change="onSuppressChange"-->
<!--                            />-->
<!--                        </div>-->
<!--                    </div>-->
<!--                </div>-->
            </a-table>
        </a-card>
    </div>
</template>

<script>
import {addViolationAnalysis, getViolationProjects} from "@/api/projects.js"
import { formatTimestamp } from "@/utils/util.js"
import {addProjectToPolicy} from "@/api/policy";
import PolicyExpandedContent from "@/views/ComponentAnalysis/Projects/Components/PolicyExpandedContent.vue";
import VulnExpandedContent from "@/views/ComponentAnalysis/Projects/Components/VulnExpandedContent.vue";

export default {
    components: {VulnExpandedContent, PolicyExpandedContent},
    computed: {
        i18nColumns() {
            return this.columns.map((column) => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey)
                }
                return column
            })
        },
        lang() {
            return this.$store.state.language.lang
        },
    },
    data() {
        return {
            columns: [
                {
                    i18nKey: "Projects.RiskType",
                    title: "风险类型",
                    dataIndex: "type",
                    sorter: true,
                    sortDirections: ["descend", "ascend"],
                },
                {
                    i18nKey: "Projects.PolicyName",
                    title: "策略名称",
                    dataIndex: "policyCondition.policy.name",
                    sorter: true,
                },

                {
                    i18nKey: "Projects.ComponentName",
                    title: "组件",
                    dataIndex: "component.name",
                    sorter: true,
                    scopedSlots: {customRender: "name"},
                },
                {
                    i18nKey: "Projects.OccurredOn",
                    title: "发行时间",
                    dataIndex: "timestamp",
                    scopedSlots: {customRender: "timestamp"},
                    sortDirections: ["descend", "ascend"],
                    sorter: true,
                },
                {
                    i18nKey: "Projects.Analysis",
                    title: "分析",
                    dataIndex: "analysis.analysisState",
                    scopedSlots: {customRender: "analysis"},
                },
                {
                    i18nKey: "Projects.Suppressed",
                    title: "屏蔽",
                    dataIndex: "analysis.suppressed",
                    scopedSlots: {customRender: "suppressed"},
                },
            ],
            projectsData: [],
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                sortOrder: "",
                sortName: "",
                searchText: "",
                suppressed: false,
                total: 0,
            },
            commit: '',
            analysisState: 'NOT_SET',
            analysisOptions: [
                {
                    label: 'NotSet',
                    value: 'NOT_SET'
                },
                {
                    label: 'Approved',
                    value: 'APPROVED'
                },
                {
                    label: 'Rejected',
                    value: 'REJECTED'
                },
            ],
            loading:false,
            isSuppressed: false,
            analysisRowData:{},
            violationRowData:{},
            commentsData:'',
            expandedRowKeys: [],
            pagination: {
              pageSize: 10,
              current: 1,
              total: 0,
              sortOrder: "",
              sortName: "",
              searchText: null,
              showLessItems: true,
            },

        }

    },
    created() {
        this.getData()
    },
    watch: {
      // 监听分页变化，每当页码变化时自动关闭展开的行
      'pagination.current'(newValue, oldValue) {
        this.expandedRowKeys = [];
      },
    },
    methods: {
        formatTimestamp,
        // 获取表格数据
        getData() {
            const uuid = this.$route.params.id
            this.loading = true;
            getViolationProjects(uuid, this.queryParams).then((res) => {
                this.queryParams.total = +res.headers["x-total-count"]
                this.projectsData = res.data
                this.loading = false;
            })

        },
        onExpandedRowsChange(keys) {
          // 更新展开的行的键值
          this.expandedRowKeys = keys;
        },
        handleChangeTable(pagination, filters, sorter) {
            if (pagination) {
                this.pagination.current = pagination.current
            }
            this.pagination.sortName = sorter.field
            if (sorter && sorter.order === "descend") {
                this.pagination.sortOrder = "desc"
            } else if (sorter && sorter.order === "ascend") {
                this.pagination.sortOrder = "asc"
            } else {
                this.pagination.sortOrder = ""
            }
            this.getData()
        },
        handleGoDetail(row) {
            console.log("row", row)
            this.$router.push(`/componentsDetail/${row.component.uuid}`)
        },
        handheTableSearch() {
            this.queryParams.pageNumber = 1
            this.getData()
        },
        getConditionVal({subject, operator, value}) {
            return `subject == ${subject} && value ${operator} ${value}`
        },
        getAuditVal(comments) {
            // console.log("comments0", comments);
            // this.violationRowData = comments;
            // this.analysisRowData = comments.analysis ? comments.analysis : {};
            // console.log("analysisRowData", this.analysisRowData);
            // console.log("analysisComments", this.analysisRowData.analysisComments);
            if (!this.analysisRowData || !this.analysisRowData.analysisComments) {
                return;
            }
            //console.log("analysisRowData", this.analysisRowData);
            let comment = ''
            this.analysisRowData.analysisComments.forEach(item => {
                // 保持返回样式，勿动
                comment +=
                    `${item.commenter} - ${this.formatTimestamp(item.timestamp, true, this.lang)}
${item.comment}
`
            })
            this.commentsData = comment;
            return comment
        },

        onSuppressChange(checked) {
            this.isSuppressed =checked;
            let data = {
                "policyViolation": this.violationRowData.uuid,
                "component": this.violationRowData.component.uuid,
                "analysisState": this.analysisState,
                "suppressed": checked
            }
            addViolationAnalysis(data).then((res) => {
                if (res.status === 200) {
                    this.$notification.success({
                        message: this.$t('Projects.UpdateMessage'),
                        description: "",
                    });
                    this.getData();
                }
            }).catch(error => {
                this.$message.error(error.response.data)
            }).finally(() => {
            });
        },

        addCommit(params) {
            let data = {
                "policyViolation": params.uuid,
                "component": params.component.uuid,
                "analysisState": this.analysisState,
                "comment": this.commit,
                "suppressed": this.isSuppressed
            }
            addViolationAnalysis(data).then((res) => {
                if (res.status === 200) {
                    this.$notification.success({
                        message: this.$t('Projects.UpdateMessage'),
                        description: "",
                    });
                    let result = res.data;
                    //console.log("res", result);
                    //this.violationRowData = result;
                    //console.log(" this.violationRowData", this.violationRowData);
                    this.analysisRowData = result ? result : {};
                    this.commit='';
                    //console.log("this.analysisRowData",  this.analysisRowData );
                    this.getAuditVal()
                }
            }).catch(error => {
                this.$message.error(error.response.data)
            }).finally(() => {
            });
        },

        handleExpand(expanded, record) {
            if (expanded) {
              this.$nextTick(() => {
                this.expandedRowKeys.push(record.key);
              });
            }else {
              this.$nextTick(() => {
                this.$refs.PolicyExpanded.clone()
              });
              const index = this.expandedRowKeys.indexOf(record.key);
              if (index !== -1) {
                this.expandedRowKeys.splice(index, 1);
              }
            }
        },
        // handleExpandedRowsChange(expandedRowKeys){
        //     // console.log("expandedRowKeys", expandedRowKeys);
        //
        //   this.$nextTick(() => {
        //     console.log(this.$refs.PolicyExpanded)
        //    let data = this.projectsData.filter(item => {
        //       if (item.uuid === expandedRowKeys[expandedRowKeys.length - 1]) {
        //         this.violationRowData = item;
        //         this.analysisRowData = item.analysis ? item.analysis : {};
        //       }
        //     })
        //     this.$refs.PolicyExpanded.open(expandedRowKeys)
        //   });
        // },
        handleAnalysisChange(value){
            this.analysisState = value;
            let data = {
                "policyViolation": this.violationRowData.uuid,
                "component": this.violationRowData.component.uuid,
                "analysisState": this.analysisState,
                "suppressed":  this.isSuppressed
            }
            addViolationAnalysis(data).then((res) => {
                if (res.status === 200) {
                    this.$notification.success({
                        message: this.$t('Projects.UpdateMessage'),
                        description: "",
                    });
                    this.getData();
                }
            }).catch(error => {
                this.$message.error(error.response.data)
            }).finally(() => {
            });
        }
    }
}
</script>

<style lang="scss" scoped>
.search {
    height: 50px;
}

.mx-25 .ant-row-flex {
    flex-wrap: wrap;
}

.v-search {
    max-width: 200px;
    width: 170px;
    min-width: 150px;
    margin-left: 5px;
    margin-bottom: 8px;
}

::v-deep .is-expand {
    transform: rotate(90deg);
    transition: all 0.5s;
}
::v-deep .disabled-textarea {
    background: #ffffff;
    width: 50%;
    border: 1px solid #E8E8E8;
    border-radius: 4px;

    .ant-input[disabled]:hover {
        border-color: transparent;
    }

    .ant-input[disabled] {
        border-color: transparent;
        background: transparent;
        color: #092943;
    }
}

.textarea-label {
    display: inline-block;
    height: 24px;
    line-height: 24px;
    padding: 0 20px;
    font-weight: 600;
    font-size: 14px;
    color: #FFFFFF;
    background: #007AFF;
    border-bottom-right-radius: 16px;
}

.commit {
    padding: 10px 20px;
    background: #FFFFFF;
    border: 1px solid #D8DDE4;

    .commit-label {
        display: inline-block;
        font-weight: 600;
        font-size: 16px;
        color: #324569;
        padding: 3px 10px;
        border-bottom: solid 2px #1890FF;
    }

    .add-commit {
        background: rgba(0, 144, 255, 0.1);
        color: #0090FF;
        box-shadow: none;
        border: none;
    }
}

::v-deep .analysis .ant-select-selection{
    background: #0090FF;
    color: #FFFFFF;
    height: 32px;

    .ant-select-selection-selected-value {
        padding-right: 10px;
        height: 32px;
        line-height: 30px;
    }

    .ant-select-arrow-icon {
        color: #FFFFFF;
    }
}
</style>
