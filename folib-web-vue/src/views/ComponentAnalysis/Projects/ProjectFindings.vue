<template>
    <div class="wrapper">
        <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
            <div class="mx-25 search">
                <a-col :span="24" class="text-right">
                    <a-input-search
                        :placeholder="$t('Projects.EnterVulnerabilityNumber')"
                        class="v-search"
                        v-model="queryParams.searchText"
                        @search="handheTableSearch()"
                    />
                </a-col>
            </div>
            <a-table
                rowKey="uuid"
                @expand="handleExpand"
                class="mt-20"
                :columns="i18nColumns"
                :data-source="projectsData"
                @change="handleChangeTable"
                :pagination="{
          pageSize: queryParams.pageSize,
          current: queryParams.pageNumber,
          total: queryParams.total || 0,
          showLessItems: true,
        }"
            >
                <template slot="name" slot-scope="name, row">
                    <a-button type="link" @click="handleGoCom(row)">
                        {{ name }}
                    </a-button>
                </template>
                <template slot="vulnId" slot-scope="vulnId, row">
                    <a-button type="link" @click="handleGoVul(row)">
                        {{ vulnId }}
                    </a-button>
                </template>

                <template slot="published" slot-scope="published">
                    {{
                        typeof published !== "undefined" ? formatTimestamp(published, true, $t('Projects.FormatTime')) : "-"
                    }}
                </template>

                <template slot="cweId" slot-scope="cweId, row">
                    {{   typeof cweId !== "undefined" ? 'CWE-'+cweId : "-" }}
                </template>

                <!-- <template slot="cwes" slot-scope="cwes"> {{}} </template> -->
                <template slot="severity" slot-scope="severity">
                    <div class="table-avatar-info" v-if="severity">
                        <a-avatar
                            v-if="['CRITICAL', 'MEDIUM', 'HIGH', 'LOW'].indexOf(severity) != -1"
                            :size="24"
                            :src="'images/folib/' + severity.toLowerCase() + '.svg'"
                        />
                        <a-avatar v-else shape="circle" :size="24">{{ severity.slice(0, 1) }}</a-avatar>
                        <div class="avatar-info">
                            <p class="mb-0 text-dark">
                                {{
                                    severity === "CRITICAL"
                                        ? "严重"
                                        : severity === "MEDIUM"
                                            ? "中危"
                                            : severity === "HIGH"
                                                ? "高危"
                                                : severity === "LOW"
                                                    ? "低危"
                                                    : severity === "UNASSIGNED" ? "未指定" : severity
                                }}
                            </p>
                        </div>
                    </div>
                </template>
                <template slot="analysis" slot-scope="analysis">
                    {{ analysis ?   analysisValue(analysis) : $t(`Projects.ANALYSIS_NOT_SET`) }}
                </template>
                <template slot="suppressed" slot-scope="suppressed">
                    {{ suppressed ? $t(`Projects.Suppressed_${suppressed}`) : $t(`Projects.Suppressed_false`) }}
                </template>

                <div slot="expandedRowRender" slot-scope="record" style="margin: 0">
                    <div class="by-flex by-row-between by-col-top">
                        <div class="disabled-textarea by-m-r-20">
                            <div class="textarea-label">{{ $t('Projects.Description') }}</div>
                            <a-input
                                :defaultValue="record.vulnerability.description"
                                type="textarea"
                                disabled
                                :rows="6"
                            ></a-input>
                        </div>
                        <div class="disabled-textarea">
                            <div class="textarea-label">{{ $t('Projects.AuditTrail') }}</div>
                            <a-input
                                type="textarea"
                                v-model="commentsData"
                                :defaultValue="commentsData"
                                disabled
                                :rows="6"
                            ></a-input>
                        </div>
                    </div>
                    <div class="commit by-m-t-20">
                        <div class="commit-label">{{ $t('Projects.Commit') }}</div>
                        <a-input
                            :placeholder="$t('Projects.CommitPlaceholder')"
                            type="textarea"
                            :rows="6"
                            :defaultValue="comment"
                            v-model="comment"
                            class="by-m-t-10 by-m-b-10"

                        ></a-input>
                        <a-button class="add-commit" @click="addCommit">{{
                                $t('Projects.AddCommit')
                            }}
                        </a-button>
                    </div>
                    <div class="by-flex by-m-t-20">
                        <div class="by-flex">
                            <a-tooltip placement="topLeft" :title="$t('Projects.AnalysisPrompt')"
                                       arrow-point-at-center>
                                <span class="by-m-r-10">{{ $t('Projects.Analysis') }}</span>
                                <a-select
                                    v-model="analysisState"
                                    class="analysis"
                                    @change="handleAnalysisChange"
                                >
                                    <a-select-option
                                        v-for="(item, i) in analysisOptions"
                                        :value="item.value"
                                        :key="i"
                                    >
                                        {{ $t(`Projects.${item.label}`) }}
                                    </a-select-option>
                                </a-select>
                            </a-tooltip>
                        </div>
                        <div class="by-m-l-45 by-flex">
                            <span class="by-m-r-10">{{ $t('Projects.Suppressed') }}</span>
                            <a-switch
                                v-model="isSuppressed"
                                checked-children="是"
                                un-checked-children="否"
                                @change="handleSuppressedChange"
                            />
                        </div>
                        <div class="by-m-l-45 by-flex">
                            <a-tooltip placement="topLeft" :title="$t('Projects.JustificationPrompt')"
                                       arrow-point-at-center>
                                <span class="by-m-r-10">{{ $t('Projects.Justification') }}</span>
                                <a-select
                                    v-model="justificationState"
                                    class="analysis"
                                    :disabled="isJustification"
                                    @change="handleJustificationChange"
                                >
                                    <a-select-option
                                        v-for="(item, i) in justificationOptions"
                                        :value="item.value"
                                        :key="i"
                                    >
                                        {{ $t(`Projects.${item.label}`) }}
                                    </a-select-option>
                                </a-select>
                            </a-tooltip>
                        </div>
                        <div class="by-m-l-45 by-flex">
                            <a-tooltip placement="topLeft" :title="$t('Projects.VendorResponsePrompt')"
                                       arrow-point-at-center>
                                <span class="by-m-r-10">{{ $t('Projects.VendorResponse') }}</span>
                                <a-select
                                    v-model="vendorResponseState"
                                    class="analysis"
                                    @change="handleVendorResponseChange"
                                >
                                    <a-select-option
                                        v-for="(item, i) in vendorResponseOptions"
                                        :value="item.value"
                                        :key="i"
                                    >
                                        {{ $t(`Projects.${item.label}`) }}
                                    </a-select-option>
                                </a-select>
                            </a-tooltip>
                        </div>

                    </div>
                    <div class="commit by-m-t-20">
                        <div class="commit-label">{{ $t('Projects.Details') }}</div>
                        <a-tooltip placement="topLeft" :title="$t('Projects.DetailsPrompt')"
                                   arrow-point-at-center>
                            <a-input
                                :placeholder="$t('Projects.DetailsPlaceholder')"
                                type="textarea"
                                :rows="6"
                                class="by-m-t-10 by-m-b-10"
                                v-model="analysisDetails"
                            ></a-input>
                            <a-button class="add-commit" @click="addCommit">{{
                                    $t('Projects.UpdateDetails')
                                }}
                            </a-button>
                        </a-tooltip>
                    </div>
                </div>
            </a-table>
        </a-card>
    </div>
</template>

<script>
import {
    addProjectProperty,
    addVulnerabilitiesAnalysis,
    getFindingProject,
    getVulnerabilitiesAnalysis
} from "@/api/projects.js"
import {formatTimestamp} from "@/utils/util.js"

export default {
    components: {},
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
                    i18nKey: "Projects.ComponentName",
                    title: "组件",
                    dataIndex: "component.name",
                    sorter: (a, b) => a.component.name.length - b.component.name.length,
                    scopedSlots: {customRender: "name"},
                    sortDirections: ["descend", "ascend"],
                },
                {
                    i18nKey: "Projects.Version",
                    title: "版本",
                    dataIndex: "component.version",
                    //   sorter: (a, b) => a.component.version - b.component.version,
                    sortDirections: ["descend", "ascend"],
                },
                {
                    i18nKey: "Projects.Group",
                    title: "组",
                    dataIndex: "component.group",
                    sorter: (a, b) => a.component.group.length - b.component.group.length,
                    sortDirections: ["descend", "ascend"],
                },
                {
                    i18nKey: "Projects.Vulnerability",
                    title: "漏洞编号",
                    dataIndex: "vulnerability.vulnId",
                    // sorter: true,
                    scopedSlots: {customRender: "vulnId"},
                    // sortDirections: ["descend", "ascend"],
                },

                {
                    i18nKey: "Projects.Aliases",
                    // title: this.$t("message.aliases"),
                    dataIndex: "vulnerability.aliases",
                },
                {
                    i18nKey: "Projects.CWE",
                    // title: this.$t("message.cwe"),
                    dataIndex:  "vulnerability.cweId",
                    scopedSlots: {customRender: "cweId"},
                    // sortDirections: ["descend", "ascend"],
                    // customRender(value, row, index) {
                    //     if (typeof value !== "undefined") {
                    //         let label = ""
                    //         for (let i = 0; i < value.length; i++) {
                    //             // label += common.formatCweShortLabel(value[i].cweId, value[i].name)
                    //             if (i < value.length - 1) label += ", "
                    //         }
                    //         return label
                    //     }
                    // },
                },

                {
                    i18nKey: "Projects.Severity",
                    title: "漏洞类别",
                    dataIndex: "vulnerability.severity",
                    scopedSlots: {customRender: "severity"},
                 },
                // {
                //     i18nKey: "Projects.Analyzer",
                //     title: this.$t("message.analyzer"),
                //     dataIndex: "attribution.analyzerIdentity",
                //     sortDirections: ["descend", "ascend"],
                //     // customRender(value, row, index) {
                //     //   return common.formatAnalyzerLabel(
                //     //     row.attribution.analyzerIdentity,
                //     //     row.vulnerability.source,
                //     //     row.vulnerability.vulnId,
                //     //     row.attribution.alternateIdentifier,
                //     //     row.attribution.referenceUrl
                //     //   )
                //     // },
                // },
                {
                    i18nKey: "Projects.AttributedOn",
                    title: "归属于",
                    dataIndex: "attribution.attributedOn",
                    scopedSlots: {customRender: "published"},
                    sortDirections: ["descend", "ascend"],
                    // sorter: true,
                },
                {
                    i18nKey: "Projects.Analysis",
                    title: "分析",
                    dataIndex: "analysis.analysisState",
                    scopedSlots: {customRender: "analysis"},
                },
                {
                    i18nKey: "Projects.Suppressed",
                    title: "抑制",
                    dataIndex: "analysis.isSuppressed",
                    scopedSlots: {customRender: "suppressed"},
                },
            ],
            projectsData: [],
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                sortOrder: "",
                sortName: "",
                searchText: null,
                total: 0,
            },
            analysisOptions: [
                {
                    label: 'NotSet',
                    value: 'NOT_SET'
                },
                {
                    label: 'Exploitable',
                    value: 'EXPLOITABLE'
                },
                {
                    label: 'InTriage',
                    value: 'IN_TRIAGE'
                },
                {
                    label: 'Resolved',
                    value: 'REJECTED'
                },
                {
                    label: 'FalsePositive',
                    value: 'FALSE_POSITIVE'
                },
                {
                    label: 'NotAffected',
                    value: 'NOT_AFFECTED'
                }
            ],
            vendorResponseOptions: [
                {
                    label: 'NotSet',
                    value: 'NOT_SET'
                },
                {
                    label: 'CanNotFix',
                    value: 'CAN_NOT_FIX'
                },
                {
                    label: 'WillNotFix',
                    value: 'WILL_NOT_FIX'
                },
                {
                    label: 'Update',
                    value: 'update'
                },
                {
                    label: 'Rollback',
                    value: 'ROLLBACK'
                },
                {
                    label: 'WorkaroundAvailable',
                    value: 'WORKAROUND_AVAILABLE'
                },
            ],
            justificationOptions: [
                {
                    label: 'NotSet',
                    value: 'NOT_SET'
                },
                {
                    label: 'CodeNotPresent',
                    value: 'CODE_NOT_PRESENT'
                },
                {
                    label: 'CodeNotReachable',
                    value: 'CODE_NOT_REACHABLE'
                },
                {
                    label: 'RequiresConfiguration',
                    value: 'REQUIRES_CONFIGURATION'
                },
                {
                    label: 'RequiresDependency',
                    value: 'REQUIRES_DEPENDENCY'
                },
                {
                    label: 'RequiresEnvironment',
                    value: 'REQUIRES_ENVIRONMENT'
                },
                {
                    label: 'ProtectedByCompiler',
                    value: 'PROTECTED_BY_COMPILER'
                },
                {
                    label: 'ProtectedAtRuntime',
                    value: 'PROTECTED_AT_RUNTIME'
                },
                {
                    label: 'ProtectedAtPerimeter',
                    value: 'PROTECTED_AT_PERIMETER'
                }
            ],

            analysisState: 'NOT_SET',
            justificationState: 'NOT_SET',
            vendorResponseState: 'NOT_SET',
            analysisDetails: '',
            isJustification: true,
            isSuppressed: false,
            projectUuid: null,
            componentUuid: null,
            vulnerabilityUuid: null,
            analysisComments: [],
            commentsData: '',
            comment: '',
        }
    },
    created() {
        this.getData()
    },
    methods: {
        formatTimestamp,
        // 获取表格数据
        getData() {
            const uuid = this.$route.params.id
            getFindingProject(uuid, this.queryParams).then((res) => {
                this.queryParams.total = +res.headers["x-total-count"]
                this.projectsData = res.data
            })
        },
        handleChangeTable(pagination, filters, sorter) {
            if (pagination) {
                this.queryParams.pageNumber = pagination.current
            }
            this.queryParams.sortName = sorter.field
            if (sorter && sorter.order === "descend") {
                this.queryParams.sortOrder = "desc"
            } else if (sorter && sorter.order === "ascend") {
                this.queryParams.sortOrder = "asc"
            } else {
                this.queryParams.sortOrder = ""
            }
            this.getData()
        },
        handleGoCom(row) {
            console.log(row)
            this.$router.push(`/componentDetail/${row.component.uuid}`)
        },

        handleGoVul(row) {
            this.$router.push(`/vulnerabilities/vulnerabilitiesDetail/${row.vulnerability.vulnId}?source=2`)
        },
        handheTableSearch() {
            this.queryParams.pageNumber = 1
            this.getData()
        },

        handleExpand(expanded, record) {
            if (expanded) {
                this.isSuppressed = record.analysis.isSuppressed;
                this.analysisState = record.analysis.state ? record.analysis.state : this.analysisState;
                this.projectUuid = record.component.project;
                this.componentUuid = record.component.uuid;
                this.vulnerabilityUuid = record.vulnerability.uuid;
                getVulnerabilitiesAnalysis(this.projectUuid, this.componentUuid, this.vulnerabilityUuid).then((res) => {
                    if (res.status === 200) {
                        this.isSuppressed = res.data.suppressed;
                        this.analysisState = res.data.state ? res.data.state : this.analysisState;
                        this.justificationState = res.data.analysisJustification ? res.data.analysisJustification : this.justificationState;
                        this.vendorResponseState = res.data.analysisResponse ? res.data.analysisResponse : this.vendorResponseState;
                        this.analysisDetails = res.data.analysisDetails ? res.data.analysisDetails : this.analysisDetails;
                        this.analysisComments = res.data.analysisComments;
                        this.getAuditVal();
                    }
                }).finally(() => {
                    this.loading = false
                });

            }
        },

        handleSuppressedChange() {
            this.addCommit();
        },
        handleJustificationChange(){
            this.addCommit();
        },
        handleVendorResponseChange(){
            this.addCommit();
        },
        handleAnalysisChange() {
            if (this.analysisState === 'NOT_AFFECTED') {
                this.isJustification = false
            } else {
                this.isJustification = true
            }
            this.addCommit();
        },
        getAuditVal() {
            if (!this.analysisComments || this.analysisComments.length === 0) {
                return;
            }
            //console.log("analysisRowData", this.analysisRowData);
            let comment = ''
            this.analysisComments.forEach(item => {
                // 保持返回样式，勿动
                comment +=
                    `${item.commenter} - ${this.formatTimestamp(item.timestamp, true, this.lang)}
${item.comment}
`
            })
            this.commentsData = comment;
            return comment
        },
        addCommit() {
            const data = {
                "project": this.projectUuid,
                "component": this.componentUuid,
                "vulnerability": this.vulnerabilityUuid,
                "analysisState": this.analysisState,
                "analysisJustification": this.justificationState,
                "analysisResponse": this.vendorResponseState,
                "analysisDetails": this.analysisDetails === '' ? null : this.analysisDetails,
                "comment": this.comment === '' ? null : this.comment,
                "isSuppressed": this.isSuppressed
            }
            addVulnerabilitiesAnalysis(data).then((res) => {
                if (res.status === 200) {
                    this.isSuppressed = res.data.isSuppressed ? res.data.isSuppressed : false;
                    this.analysisState = res.data.state ? res.data.state : this.analysisState;
                    this.justificationState = res.data.analysisJustification ? res.data.analysisJustification : this.justificationState;
                    this.vendorResponseState = res.data.analysisResponse ? res.data.analysisResponse : this.vendorResponseState;
                    this.analysisDetails = res.data.analysisDetails ? res.data.analysisDetails : this.analysisDetails;
                    this.analysisComments = res.data.analysisComments;
                    this.$notification.success({
                        message: this.$t('Projects.UpdateMessage'),
                        description: "",
                    });
                    this.getAuditVal();
                }
            }).finally(() => {
                this.loading = false
            });
        },
        convertToCamelCase(str) {
            // 将字符串按照下划线分割成数组
            let words = str.toLowerCase().split('_');

            // 将数组中每个单词的首字母转换成大写
            for (let i = 0; i < words.length; i++) {
                words[i] = words[i][0].toUpperCase() + words[i].slice(1);
            }

            // 将数组中第一个单词的首字母转换成小写
            words[0] = words[0][0].toLowerCase() + words[0].slice(1);

            // 将数组中的单词连接起来并返回
            return words.join('');
        },
        analysisValue(key){
            let toKey = convertToCamelCase(key);
           return  this.$t(`Projects.${toKey}`)
        }
    },
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

::v-deep .analysis .ant-select-selection {
    background: #0090FF;
    color: #FFFFFF;
    height: 32px;
    width: 150px;

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
