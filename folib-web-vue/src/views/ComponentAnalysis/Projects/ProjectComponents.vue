<template>
    <div class="wrapper">
        <a-card :bordered="false" style="margin-top: 20px; margin-bottom: 20px">
            <div class="mx-25 search">
                <div style="display: flex; justify-content: space-between">
                    <div>
                        <!-- <a-button type="primary" icon="plus" @click="handleAdd" style="margin-right: 16px;">
                            {{ $t("Projects.AddComponent") }}
                        </a-button>
                        <a-button type="primary" icon="minus" @click="handleDel" :disabled="!selectedRows.length">
                            {{ $t("Projects.DelComponent") }}
                        </a-button> -->
                    </div>
                    <a-input-search
                        :placeholder="$t('Projects.componentNameQuery')"
                        class="v-search"
                        v-model="queryParams.searchText"
                        @search="handheTableSearch()"
                    />
                </div>
            </div>
            <a-table
                rowKey="uuid"
                class="mt-20"
                :columns="i18nColumns"
                :data-source="projectsData"
                :row-selection="{ onChange: handleSelectionChange }"
                @change="handleChangeTable"
                :pagination="{
          pageSize: queryParams.pageSize,
          current: queryParams.pageNumber,
          total: queryParams.total || 0,
          showLessItems: true,
        }"
            >
                <template slot="name" slot-scope="name, row">
                    <a-button type="link" @click="handleGoDetail(row)">
                        {{ name }}
                    </a-button>
                </template>
                <template slot="license" slot-scope="license, row">
                    <a-button type="link" @click="handleGoLicense(row)" v-if="row.licenseId">
                        {{ row.licenseId }}
                    </a-button>
                    <span v-if="!row.licenseId" style="padding: 0 16px">
            {{ license }}
          </span>
                </template>
                <!-- <template slot="policy" slot-scope="row">
                  {{ row }}
                </template> -->
                <template slot="vulnerabilities" slot-scope="vulnerabilities, row">
                    <a-tag color="#f86c6b">{{ row.metrics && row.metrics.critical ? row.metrics.critical : 0 }}</a-tag>
                    <a-tag color="#fd8c00">{{ row.metrics && row.metrics.high ? row.metrics.high : 0 }}</a-tag>
                    <a-tag color="#ffc107">{{ row.metrics && row.metrics.medium ? row.metrics.medium : 0 }}</a-tag>
                    <a-tag color="#4dbd74"> {{ row.metrics && row.metrics.low ? row.metrics.low : 0 }}</a-tag>
                    <a-tag color="#777777"> {{ row.metrics && row.metrics.unassigned ? row.metrics.unassigned : 0 }}</a-tag>
                </template>
            </a-table>
        </a-card>
        <a-modal :title="$t('Projects.ComponentDetails')" :visible="visible" @cancel="onClose" width="60%">
            <a-tabs v-model="tabActive">
                <a-tab-pane key="1" :tab="$t('Projects.Identity')">
                    <a-form-model :model="addForm1" :rules="addRules1" ref="addForm1">
                        <a-form-model-item :label="$t('Projects.ComponentName')" prop="name">
                            <a-input v-model="addForm1.name"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.Version')" prop="version">
                            <a-input v-model="addForm1.version"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.NamespaceGroupVendor')" prop="group">
                            <a-input v-model="addForm1.group"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.Purl')" prop="purl">
                            <a-input v-model="addForm1.purl"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.Cpe')" prop="cpe">
                            <a-input v-model="addForm1.cpe"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.SwidTagId')" prop="swidTagId">
                            <a-input v-model="addForm1.swidTagId"/>
                        </a-form-model-item>
                    </a-form-model>
                </a-tab-pane>
                <a-tab-pane key="2" :tab="$t('Projects.Extended')">
                    <a-form-model :model="addForm2" ref="addForm2" :rules="addRules2">
                        <a-form-model-item :label="$t('Projects.Classifier')" prop="classifier">
                            <a-select v-model="addForm2.classifier">
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
                        <a-form-model-item :label="$t('Projects.Filename')" prop="filename">
                            <a-input v-model="addForm2.filename"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.Description')" prop="description">
                            <a-textarea v-model="addForm2.description" :auto-size="{ minRows: 4 }"/>
                        </a-form-model-item>
                    </a-form-model>
                </a-tab-pane>
                <a-tab-pane key="3" :tab="$t('Projects.Legal')">
                    <a-form-model :model="addForm3" ref="addForm3">
                        <a-form-model-item :label="$t('Projects.License')" prop="license">
                            <a-select v-model="addForm3.license">
                                <a-select-option v-for="(item, index) in licenses" :value="item.licenseId" :key="index">
                                    {{ item.name }}
                                </a-select-option>
                            </a-select>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.Copyright')" prop="copyright">
                            <a-textarea v-model="addForm3.copyright" :auto-size="{ minRows: 4 }"/>
                        </a-form-model-item>
                    </a-form-model>
                </a-tab-pane>
                <a-tab-pane key="4" :tab="$t('Projects.Hashes')">
                    <a-form-model :model="addForm4" ref="addForm4">
                        <a-form-model-item :label="$t('Projects.MD5')" prop="md5">
                            <a-input v-model="addForm4.md5"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.SHA1')" prop="sha1">
                            <a-input v-model="addForm4.sha1"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.SHA256')" prop="sha256">
                            <a-input v-model="addForm4.sha256"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.SHA512')" prop="sha512">
                            <a-input v-model="addForm4.sha512"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.SHA3256')" prop="sha3_256">
                            <a-input v-model="addForm4.sha3_256"/>
                        </a-form-model-item>
                        <a-form-model-item :label="$t('Projects.SHA3512')" prop="sha3_512">
                            <a-input v-model="addForm4.sha3_512"/>
                        </a-form-model-item>
                    </a-form-model>
                </a-tab-pane>
                <a-tab-pane key="5" :tab="$t('Projects.Notes')">
                    <a-form-model :model="addForm5" ref="addForm5">
                        <a-form-model-item :label="$t('Projects.Notes')" prop="notes">
                            <a-textarea v-model="addForm5.notes" :auto-size="{ minRows: 4 }"/>
                        </a-form-model-item>
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
import {getProjectsComponents, getLicenseConcise, addProjectComponent} from "@/api/projects.js"
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
    },
    data() {
        return {
            tabActive: '1',
            licenses: [],
            addRules1: {
                name: [
                    {
                        required: true,
                        message: () => this.$t("Projects.NameRequired"),
                        trigger: "blur",
                    },
                ],
                version: [
                    {
                        required: true,
                        message: () => this.$t("Projects.VersionRequired"),
                        trigger: "blur",
                    },
                ],
            },
            addRules2: {
                classifier: [
                    {
                        required: true,
                        message: () => this.$t("Projects.ClassifierRequired"),
                        trigger: "blur",
                    },
                ],
            },
            addForm1: {
                name: '',
                version: '',
                group: '',
                purl: '',
                cpe: '',
                swidTagId: ''
            },
            addForm2: {
                classifier: '',
                filename: '',
                description: ''
            },
            addForm3: {
                license: '',
                copyright: ''
            },
            addForm4: {
                md5: '',
                sha1: '',
                sha256: '',
                sha512: '',
                sha3_256: '',
                sha3_512: '',
            },
            addForm5: {
                notes: ''
            },
            visible: false,
            loading: false,
            selectedRows: [],
            columns: [
                {
                    title: "组件名称",
                    i18nKey: "Projects.ComponentName",
                    dataIndex: "name",
                    sorter: true,
                    scopedSlots: {customRender: "name"},
                    sortDirections: ["descend", "ascend"],
                },
                {
                    title: "版本",
                    i18nKey: "Projects.Version",
                    dataIndex: "version",
                    sortDirections: ["descend", "ascend"],
                    sorter: true,
                    width: "150px",
                },
                {
                    i18nKey: "Projects.LatestVersion",
                    title: "最新版本",
                    dataIndex: "repositoryMeta.latestVersion",
                    width: "150px",
                },
                {
                    i18nKey: "Projects.Group",
                    title: "组",
                    dataIndex: "group",
                    sorter: true,
                    sortDirections: ["descend", "ascend"],
                },
                // {
                //     i18nKey: "Projects.Internal",
                //     title: "内部",
                //     dataIndex: "isInternal",
                //     scopedSlots: {customRender: "lastBomImport"},
                //     width: "100px",
                // },
                {
                    i18nKey: "Projects.Licence",
                    title: "证书",
                    dataIndex: "license",
                    sorter: true,
                    scopedSlots: {customRender: "license"},
                    sortDirections: ["descend", "ascend"],
                    width: "230px",
                },

                {
                    i18nKey: "Projects.RiskScore",
                    title: "风险评分",
                    dataIndex: "lastInheritedRiskScore",
                    sorter: true,
                    sortDirections: ["descend", "ascend"],
                    width: "120px",
                },
                {
                    i18nKey: "Projects.Vulnerabilities",
                    title: "漏洞",
                    dataIndex: "metrics",
                    scopedSlots: {customRender: "vulnerabilities"},
                    width: "230px",
                },
            ],
            projectsData: [],
            queryParams: {
                pageNumber: 1,
                pageSize: 10,
                total: 0,
                searchText: "",
            },
        }
    },
    created() {
        this.getData()
    },
    methods: {
        formatTimestamp,
        handleDel() {

        },
        onClose() {
            if (this.$refs.addForm1 !== undefined) {
                this.$refs.addForm1.resetFields()
            }
            if (this.$refs.addForm2 !== undefined) {
                this.$refs.addForm2.resetFields()
            }
            if (this.$refs.addForm3 !== undefined) {
                this.$refs.addForm3.resetFields()
            }
            if (this.$refs.addForm4 !== undefined) {
                this.$refs.addForm4.resetFields()
            }
            if (this.$refs.addForm5 !== undefined) {
                this.$refs.addForm5.resetFields()
            }
            this.tabActive = '1'
            this.visible = false
        },
        handleAdd() {
            getLicenseConcise().then((res) => {
                this.licenses = res.data
                console.log(res, 'df')
            })
            this.visible = true
        },
        handleSureAdd() {
            this.$refs.addForm1.validate((valid) => {
                if (valid) {
                    this.$refs.addForm2.validate((valid) => {
                        if (valid) {
                            this.loading = true
                            const params = {
                                name: this.addForm1.name,
                                version: this.addForm1.version,
                                group: this.addForm1.group,
                                purl: this.addForm1.purl,
                                cpe: this.addForm1.cpe,
                                swidTagId: this.addForm1.swidTagId,
                                classifier: this.addForm2.classifier,
                                filename: this.addForm2.filename,
                                description: this.addForm2.description,
                                license: this.addForm3.license,
                                copyright: this.addForm3.copyright,
                                md5: this.addForm4.md5,
                                sha1: this.addForm4.sha1,
                                sha256: this.addForm4.sha256,
                                sha512: this.addForm4.sha512,
                                sha3_256: this.addForm4.sha3_256,
                                sha3_512: this.addForm4.sha3_512,
                                notes: this.addForm5.notes
                            }
                            addProjectComponent(this.$route.params.id, params).then((res) => {
                                console.log(res)
                                this.getData()
                                this.$notification.success({
                                    message: "新增成功",
                                    description: "",
                                })
                                this.onClose()
                            }).catch((error) => {
                                this.$notification.error({
                                    message: "新增失败",
                                    description: "",
                                })
                            }).finally(() => {
                                this.loading = false
                            })
                        } else {
                            return false
                        }
                    })
                } else {
                    return false
                }
            })
        },
        handleSelectionChange(selectedRowKeys, selectedRows) {
            this.selectedRows = selectedRows
            console.log(this.selectedRows, 'dfdf')
        },
        // 获取表格数据
        getData() {
            const uuid = this.$route.params.id
            getProjectsComponents(uuid, this.queryParams).then((res) => {
                this.queryParams.total = +res.headers["x-total-count"]
                this.projectsData = res.data === "" ? [] : res.data
                this.projectsData = this.projectsData.map((item) => {
                    if (item.resolvedLicense) {
                        item.licenseId = item.resolvedLicense.licenseId
                    }
                    return item
                })
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
            this.$router.push(`/componentDetail/${row.uuid}`)
        },
        handleGoLicense(row) {
            console.log(row)
            this.$router.push(`/licenses/licensesDetail/${row.licenseId}`)
        },
        handheTableSearch() {
            this.queryParams.pageNumber = 1
            this.getData()
        },
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
</style>
