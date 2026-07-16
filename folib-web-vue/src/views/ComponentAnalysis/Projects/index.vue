<template>
    <div class="wrapper">
        <!-- <HeaderEcharts></HeaderEcharts> -->
        <a-tabs
            class="tabs-sliding"
            :default-active-key="1"
            :activeKey="tabActiveKey"
            @change="tabChange($event)"
            >
            <a-tab-pane :key="1" :tab="$t('Projects.Repository')">
                <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
                    <div class="mx-25 search">
                        <a-col :span="24" class="text-right">                        
                            <a-input-search
                                :placeholder="$t('Projects.RepositoryNameQuery')"
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
                        @change="handleChangeTable"
                        childrenColumnName="childrenColumnName"
                        :pagination="{
                pageSize: queryParams.pageSize,
                current: queryParams.pageNumber,
                total: queryParams.total || 0,
                showLessItems: true,
                }"
                    >
                        <template slot="name" slot-scope="name, row">
                            <a-tooltip placement="rightTop">
                                <template slot="title">
                                    {{ name }}
                                </template>
                                <a-button type="link" @click="handleGoDetail(row)">
                                    {{    omitName(name) }}
                                </a-button>
                            </a-tooltip>

                        </template>
                        <template slot="classifier" slot-scope="classifier">
                            <a-button type="link">
                                {{ classifier }}
                            </a-button>
                        </template>
                        <template slot="lastBomImport" slot-scope="lastBomImport">
                            {{
                                typeof lastBomImport === "number" ? formatTimestamp(lastBomImport, true, $t('Projects.FormatTime')) : "-"
                            }}
                        </template>
                        <template slot="vulnerabilities" slot-scope="vulnerabilities, row">
                            <a-tag color="#f86c6b">{{ row.metrics && row.metrics.critical ? row.metrics.critical : 0 }}</a-tag>
                            <a-tag color="#fd8c00">{{ row.metrics && row.metrics.high ? row.metrics.high : 0 }}</a-tag>
                            <a-tag color="#ffc107">{{ row.metrics && row.metrics.medium ? row.metrics.medium : 0 }}</a-tag>
                            <a-tag color="#4dbd74"> {{ row.metrics && row.metrics.low ? row.metrics.low : 0 }}</a-tag>
                            <a-tag color="#777777"> {{ row.metrics && row.metrics.unassigned ? row.metrics.unassigned : 0 }}</a-tag>
                        </template>
                        <template slot="lastBomImportFormat" slot-scope="lastBomImportFormat">
                            {{ !lastBomImportFormat ? "-" : lastBomImportFormat }}
                        </template>

                        <template slot="policyViolations" slot-scope="policyViolations">
                            <a-tooltip placement="left" >
                                <template slot="title" >
                                    <div style="width: 120px;">
                                        <p class="policyPromptTitle">{{ $t('Projects.PolicyType') }}</p>
                                        <p class="policyPromptText">{{ $t('Projects.LicenseViolations') }}：{{
                                                policyViolations && policyViolations.policyViolationsLicenseTotal ? policyViolations.policyViolationsLicenseTotal : 0
                                            }} </p>
                                        <p class="policyPromptText">{{ $t('Projects.OperationalViolations') }}： {{
                                                policyViolations && policyViolations.policyViolationsOperationalTotal ? policyViolations.policyViolationsOperationalTotal : 0
                                            }}</p>
                                        <p class="policyPromptText">{{ $t('Projects.SecurityViolations') }}：{{
                                                policyViolations &&  policyViolations.policyViolationsSecurityTotal ? policyViolations.policyViolationsSecurityTotal : 0
                                            }}</p>
                                        <p class="policyPromptTitle">{{ $t('Projects.ViolationState') }}</p>
                                        <p class="policyPromptText">{{ $t('Projects.InformationalViolations') }}: {{
                                                policyViolations &&  policyViolations.policyViolationsInfo ? policyViolations.policyViolationsInfo : 0
                                            }}</p>
                                        <p class="policyPromptText">{{ $t('Projects.ViolationWarnings') }}：{{
                                                policyViolations && policyViolations.policyViolationsWarn ? policyViolations.policyViolationsWarn : 0
                                            }}</p>
                                        <p class="policyPromptText">{{ $t('Projects.ViolationFailures') }}：{{
                                                policyViolations && policyViolations.policyViolationsFail ? policyViolations.policyViolationsFail : 0
                                            }}</p>
                                        <p class="policyPromptTitle">{{ $t('Projects.ViolationTotal') }}：{{
                                                policyViolations &&  policyViolations.policyViolationsTotal ? policyViolations.policyViolationsTotal : 0
                                            }}</p>
                                    </div>

                                </template>
                                <a-tag color="#0bb0d5">{{ policyViolations &&  policyViolations.policyViolationsTotal ? policyViolations.policyViolationsTotal : 0  }}</a-tag>
                            </a-tooltip>

                        </template>
                    </a-table>
                </a-card>
            </a-tab-pane>
            <a-tab-pane :key="2" :tab="$t('Projects.Artifact')">
                <ProjectChildren v-if="tabActiveKey == 2" :uuid="defaultParentId" :source="2"></ProjectChildren>
            </a-tab-pane>
        </a-tabs>

        <a-modal :title="$t('Projects.CreateProject')" :visible="visible" :width="700" @cancel="onClose">
            <a-tabs v-model="tabActive">

                <a-tab-pane key="1" :tab="$t('Projects.General')" class="scroll-container">

                    <a-form-model :model="addForm" :rules="addRules" ref="addForm" class="content">
                        <a-row :gutter="[0]">
                            <a-col class="my-auto" :span="22">
                                <a-form-model-item class="my-auto " :label="$t('Projects.ProjectName')"
                                                   prop="name">
                                    <a-input class="mt-5" property="$t('Projects.ProjectName')"
                                             v-model="addForm.name"/>
                                </a-form-model-item>
                            </a-col>
                            <a-col class="mb-auto" :span="22">
                                <a-form-model-item class="mb-auto   " :label="$t('Projects.Version')"
                                                   prop="version">
                                    <a-input class="mt-5" property="$t('Projects.Version')"
                                             v-model="addForm.version"/>
                                </a-form-model-item>
                            </a-col>
                            <a-col :span="22">
                                <a-form-model-item class="mb-auto  " :label="$t('Projects.Classifier')"
                                                   prop="classifier">
                                    <a-select property="$t('Projects.Classifier')" v-model="addForm.classifier">
                                        <a-select-option value="APPLICATION">Application</a-select-option>
                                        <a-select-option value="CONTAINER">Container</a-select-option>
                                        <a-select-option value="DEVICE">Device</a-select-option>
                                        <a-select-option value="FILE">File</a-select-option>
                                        <a-select-option value="FIRMWARE">Firmware</a-select-option>
                                        <a-select-option value="FRAMEWORK">Framework</a-select-option>
                                        <a-select-option value="LIBRARY">Library</a-select-option>
                                        <a-select-option value="OPERATING_SYSTEM">Operating system</a-select-option>
                                    </a-select>
                                </a-form-model-item>
                            </a-col>
                            <a-col :span="22">
                                <a-form-model-item class="mb-auto  " :label="$t('Projects.Parent')"
                                                   prop="parent">
                                    <a-select property="$t('Projects.Parent')" v-model="addForm.parent" show-search
                                              :default-active-first-option="false" :filter-option="false"
                                              :placeholder="$t('Projects.ParentPlaceholder')"
                                              @search="handleChange">
                                        <a-select-option v-for="(item, index) in parents" :key="item.uuid"
                                                         :value="item.uuid">
                                            {{ item.name }}
                                        </a-select-option>
                                    </a-select>
                                </a-form-model-item>
                            </a-col>
                            <a-col :span="22">
                                <a-form-model-item class="mb-auto  " :label="$t('Projects.Description')"
                                                   prop="description">
                                    <a-textarea property="$t('Projects.Description')" v-model="addForm.description"
                                                :auto-size="{ minRows: 4 }"/>
                                </a-form-model-item>
                            </a-col>
                            <a-col :span="22">
                                <a-form-model-item class="mb-auto  " :label="$t('Projects.Tags')" prop="tag">
                                    <a-input property="$t('Projects.Tags')" v-model="addForm.tag"/>
                                </a-form-model-item>
                            </a-col>
                        </a-row>

                    </a-form-model>

                </a-tab-pane>

                <a-tab-pane key="2" :tab="$t('Projects.Identity')" class="scroll-container">
                    <a-form-model :model="addForm2" ref="addForm2" class="content">
                        <a-row :gutter="[0]">
                            <a-col class="my-auto" :span="22">
                                <a-form-model-item class="mb-auto ml-25" :label="$t('Projects.ProjectName')"
                                                   prop="name">
                                    <a-input v-model="addForm.name" disabled/>
                                </a-form-model-item>
                            </a-col>
                            <a-col class="my-auto" :span="22">
                                <a-form-model-item class="mb-auto ml-25" :label="$t('Projects.Version')" prop="version">
                                    <a-input v-model="addForm.version" disabled/>
                                </a-form-model-item>
                            </a-col>
                            <a-col class="my-auto" :span="22">
                                <a-form-model-item class="mb-auto ml-25" :label="$t('Projects.NamespaceGroupVendor')"
                                                   prop="group">
                                    <a-input v-model="addForm2.group"/>
                                </a-form-model-item>
                            </a-col>
                            <a-col class="my-auto" :span="22">
                                <a-form-model-item class="mb-auto ml-25" :label="$t('Projects.Purl')" prop="purl">
                                    <a-input v-model="addForm2.purl"/>
                                </a-form-model-item>
                            </a-col>
                            <a-col class="my-auto" :span="22">
                                <a-form-model-item class="mb-auto ml-25" :label="$t('Projects.Cpe')" prop="cpe">
                                    <a-input v-model="addForm2.cpe"/>
                                </a-form-model-item>
                            </a-col>
                            <a-col class="my-auto" :span="22">
                                <a-form-model-item class="mb-auto ml-25" :label="$t('Projects.SwidTagId')"
                                                   prop="swidTagId">
                                    <a-input v-model="addForm2.swidTagId"/>
                                </a-form-model-item>
                            </a-col>
                        </a-row>
                    </a-form-model>
                </a-tab-pane>
            </a-tabs>
            <template slot="footer">
                <a-button key="back" @click="onClose">
                    {{ $t('Projects.Close') }}
                </a-button>
                <a-button key="submit" type="primary" :loading="loading" @click="handleSureAdd">
                    {{ $t('Projects.Create') }}
                </a-button>
            </template>
        </a-modal>
    </div>
</template>

<script>
import HeaderEcharts from "../Components/HeaderEcharts"
import {getProjectsList, addProject} from "@/api/projects.js"
import {formatTimestamp} from "@/utils/util.js"
import ProjectChildren from "./ProjectChildren.vue"

export default {
    components: {
        HeaderEcharts, 
        ProjectChildren
    },
    computed: {
        i18nColumns() {
            return this.columns.map((column) => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey)
                }
                return column
            })
        },
    },

    data() {
        return {
            tabActiveKey: 1,
            tabActive: '1',
            layout: {
                labelCol: {span: 5},
                wrapperCol: {span: 19},
            },
            // layout2: {
            //   labelCol: { span: 10},
            //   wrapperCol: { span: 14 },
            // },
            visible: false,
            loading: false,
            parents: [],
            addForm: {
                name: '',
                version: '',
                parent: '',
                classifier: '',
                description: '',
                tag: ''
            },
            addForm2: {
                group: '',
                purl: '',
                cpe: '',
                swidTagId: ''
            },
            addRules: {
                name: [
                    {
                        required: true,
                        message: () => this.$t("Projects.NameRequired"),
                        trigger: "blur",
                    },
                ],
                classifier: [
                    {
                        required: true,
                        message: () => this.$t("Projects.ClassifierRequired"),
                        trigger: "blur",
                    },
                ],
            },
            columns: [
                {
                    title: "仓库名称",
                    i18nKey: "Projects.RepositoryName",
                    dataIndex: "name",
                    sorter: false,
                    scopedSlots: {customRender: "name"},
                    sortDirections: ["descend", "ascend"],
                    width: "180px",
                },
                {
                    title: "上次导入清单",
                    i18nKey: "Projects.LastBOMImport",
                    dataIndex: "lastBomImport",
                    sorter: false,
                    scopedSlots: {customRender: "lastBomImport"},
                    sortDirections: ["descend", "ascend"],
                    width: "200px",
                },
                {
                    title: "清单格式",
                    i18nKey: "Projects.BOMFormat",
                    dataIndex: "lastBomImportFormat",
                    sorter: false,
                    width: "140px",
                    scopedSlots: {customRender: "lastBomImportFormat"},
                    sortDirections: ["descend", "ascend"],
                },
                {
                    title: "风险评分",
                    i18nKey: "Projects.RiskScore",
                    dataIndex: "lastInheritedRiskScore",
                    sorter: false,
                    width: "120px",
                    sortDirections: ["descend", "ascend"],
                },
                {
                    title: "违反政策",
                    i18nKey: "Projects.PolicyViolations",
                    dataIndex: "metrics",
                    sorter: false,
                    width: "150px",
                    scopedSlots: {customRender: "policyViolations"},
                    sortDirections: ["descend", "ascend"],
                },
                {
                    title: "漏洞",
                    i18nKey: "Projects.Vulnerabilities",
                    dataIndex: "vulnerabilities",
                    width: "230px",
                    scopedSlots: {customRender: "vulnerabilities"},
                },
            ],
            projectsData: [],
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                sortOrder: "",
                sortName: "",
                excludeInactive: false,
                total: 0,
            },
            defaultParentId: ''
        }
    },
    created() {
        this.getData()
    },
    methods: {
        formatTimestamp,
        tabChange(activeKey) {
            this.tabActiveKey = activeKey;
        },
        handleChange(value) {
            getProjectsList({searchText: value, excludeInactive: true}).then((res) => {
                this.parents = res.data
            })
        },
        onClose() {
            this.visible = false
            this.$refs.addForm.resetFields()
        },
        handleAdd() {
            this.visible = true
        },
        handleSureAdd() {
            this.$refs.addForm.validate((valid) => {
                if (valid) {
                    this.loading = true
                    const params = {
                        active: true,
                        name: this.addForm.name,
                        version: this.addForm.version,
                        classifier: this.addForm.classifier,
                        parent: {uuid: this.addForm.parent},
                        description: this.addForm.description,
                        tags: [{id: 0, name: this.addForm.tag}],
                        group: this.addForm2.group,
                        purl: this.addForm2.purl,
                        cpe: this.addForm2.cpe,
                        swidTagId: this.addForm2.swidTagId
                    }
                    addProject(params).then((res) => {
                        // console.log(res)
                        this.getData()
                        this.$notification.success({
                            message: "新增成功",
                            description: "",
                        })
                        this.onClose()
                    }).finally(() => {
                        this.loading = false
                    })
                } else {
                    return false
                }
            })
        },
        // 获取表格数据
        getData() {
            getProjectsList(this.queryParams).then((res) => {
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
        handleGoDetail(row) {
            this.$router.push(`/repositoryDetail/${row.uuid}?source=1`)
        },
        handheTableSearch() {
            this.queryParams.pageNumber = 1
            this.getData()
        },
        omitName(name){
            if(name.length>60){
                return name.substring(0,60)+"..."
            }else{
                return name;
            }
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

.scroll-container {
    height: 450px; /* 容器高度 */
    //width: 700px; /* 容器宽度 */
    overflow-y: auto; /* 垂直方向的内容超出容器高度时显示垂直滚动条 */
    //border: 1px solid #ccc; /* 可选：为容器添加边框 */
}

.content {
    height: 600px; /* 内容高度 */
    //width: 600px;
    // background-color: #f0f0f0;
}
.policyPromptTitle{
    font-size: 16px;
    height: 10px;
}
.policyPromptText{
    font-size: 10px;
    height: 4px;
}
</style>
