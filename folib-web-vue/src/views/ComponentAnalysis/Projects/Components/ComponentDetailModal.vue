<template>
    <a-modal
        :title="$t('Projects.ProjectDetail')"
        :visible="visible"
        :width="800"
        :dialog-style="{ top: '3vh' }"
        @ok="handleOk"
        @cancel="handleCancel"
    >
        <a-form :form="form">
            <a-tabs type="card" class="pro-tabs">
                <a-tab-pane key="1" :tab="$t('Projects.Identity')" class="tab-pane">
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.ComponentName') }}
                            <a-tooltip :title="$t('Projects.ComponentNameTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.ComponentName')"
                            v-decorator="['name', { rules: [{ required: true, message: $t('Projects.ComponentNameRequired') }] }]"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.Version') }}
                            <a-tooltip :title="$t('Projects.VersionTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.Version')"
                            v-decorator="['version', { rules: [{ required: true, message: $t('Projects.VersionRequired') }] }]"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.NGV') }}
                            <a-tooltip :title="$t('Projects.NGVTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.NGV')"
                            v-decorator="['group']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.Author') }}
                            <a-tooltip :title="$t('Projects.AuthorTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.Author')"
                            v-decorator="['author']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.PURL') }}
                            <a-tooltip :title="$t('Projects.PURLTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.PURL')"
                            v-decorator="['purl']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.CPE') }}
                            <a-tooltip :title="$t('Projects.CPETip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.CPE')"
                            v-decorator="['cpe']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.SWID') }}
                            <a-tooltip :title="$t('Projects.SWIDTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SWID')"
                            v-decorator="['swid']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.ObjectIdentifier') }}
                            <a-tooltip :title="$t('Projects.ObjectIdentifierTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            disabled
                            v-decorator="['uuid', { rules: [{ required: true, message: $t('Projects.NameRequired') }] }]"
                        />
                    </a-form-item>
                </a-tab-pane>
                <a-tab-pane key="2" :tab="$t('Projects.Extended')">
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.Classifier') }}
                            <a-tooltip :title="$t('Projects.ClassifierTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-select
                            :placeholder="$t('Projects.ClassifierRequired')"
                            v-decorator="['classifier', { rules: [{ required: true, message: $t('Projects.ClassifierRequired') }] }]"
                        >
                            <a-select-option
                                v-for="item in classifierList"
                                :key="item.value"
                                :value="item.value"
                            >
                                {{ item.label }}
                            </a-select-option>
                        </a-select>
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.Filename') }}
                            <a-tooltip :title="$t('Projects.FilenameTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.Filename')"
                            v-decorator="['filename']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0" :label="$t('Projects.Description')">
                        <a-input
                            type="textarea"
                            :rows="6"
                            v-decorator="['description']"
                        />
                    </a-form-item>
                </a-tab-pane>
                <a-tab-pane key="3" :tab="$t('Projects.Legal')">
                    <a-form-model-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.License') }}
                            <a-tooltip :title="$t('Projects.LicenseTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-select
                            :placeholder="$t('Projects.License')"
                            v-decorator="['license']"
                        >
                            <a-select-option
                                v-for="(item, index) in licenseList"
                                :value="item.licenseId"
                                :key="index"
                            >
                                {{ item.name }}
                            </a-select-option>
                        </a-select>
                    </a-form-model-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.SPDX') }}
                            <a-tooltip :title="$t('Projects.SPDXTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SPDX')"
                            v-decorator="['SPDX']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.LicenseUrl') }}
                            <a-tooltip :title="$t('Projects.LicenseUrlTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.LicenseUrl')"
                            v-decorator="['licenseUrl']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0" :label="$t('Projects.Copyright')">
                        <a-input
                            type="textarea"
                            :rows="6"
                            v-decorator="['copyright']"
                        />
                    </a-form-item>
                </a-tab-pane>
                <a-tab-pane key="4" :tab="$t('Projects.Hashes')">
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.MD5') }}
                            <a-tooltip :title="$t('Projects.HashTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.MD5')"
                            v-decorator="['md5']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.SHA1') }}
                            <a-tooltip :title="$t('Projects.HashTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SHA1')"
                            v-decorator="['sha1']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.SHA256') }}
                            <a-tooltip :title="$t('Projects.HashTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SHA256')"
                            v-decorator="['sha256']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.SHA512') }}
                            <a-tooltip :title="$t('Projects.HashTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SHA512')"
                            v-decorator="['sha512']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.SHA3256') }}
                            <a-tooltip :title="$t('Projects.HashTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SHA3256')"
                            v-decorator="['sha3256']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.SHA3512') }}
                            <a-tooltip :title="$t('Projects.HashTip')">
                                <a-icon type="info-circle"/>
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SHA3512')"
                            v-decorator="['sha3512']"
                        />
                    </a-form-item>
                </a-tab-pane>
                <a-tab-pane key="5" :tab="$t('Projects.ExternalReferences')">
                    <a-table :columns="columns" :data-source="tableData" rowKey="uuid"></a-table>
                </a-tab-pane>
                <a-tab-pane key="6" :tab="$t('Projects.Notes')">
                    <a-form-item class="by-m-b-0" :label="$t('Projects.Notes')">
                        <a-input
                            type="textarea"
                            :rows="6"
                            v-decorator="['notes']"
                        />
                    </a-form-item>
                </a-tab-pane>
            </a-tabs>
        </a-form>
        <template slot="footer">
            <!-- <a-button key="delete" type="danger" ghost @click="handleDelete">
                {{ $t(`Projects.Delete`) }}
            </a-button> -->
            <a-button key="back" @click="handleCancel">
                {{ $t(`Projects.Cancel`) }}
            </a-button>
            <!-- <a-button key="submit" type="primary" :loading="loading" @click="handleOk">
                {{ $t(`Projects.Update`) }}
            </a-button> -->
        </template>
    </a-modal>
</template>
<script>
import {deleteComponent, getComponentHeaderDetail, getLicenseConcise, updateComponent} from "@/api/projects.js"

export default {
    name: "ComponentDetailModal",
    components: {},
    data() {
        return {
            visible: false,
            form: this.$form.createForm(this, {name: 'pro_detail'}),
            classifierList: [
                {
                    label: 'Application',
                    value: 'APPLICATION'
                },
                {
                    label: 'Framework',
                    value: 'FRAMEWORK'
                },
                {
                    label: 'Library',
                    value: 'LIBRARY'
                },
                {
                    label: 'Container',
                    value: 'CONTAINER'
                },
                {
                    label: 'Operating system',
                    value: 'OPERATING_SYSTEM'
                },
                {
                    label: 'Device',
                    value: 'DEVICE'
                },
                {
                    label: 'Firmware',
                    value: 'FIRMWARE'
                },
                {
                    label: 'File',
                    value: 'FILE'
                },
            ],
            licenseList: [],
            tableData: [],
            columns: [
                {
                    title: () => this.$t('Projects.URL'),
                    dataIndex: 'url',
                    ellipsis: true
                },
                {
                    title: this.$t('Projects.Type'),
                    dataIndex: 'type',
                    ellipsis: true
                },
            ],
            loading: false,
            component: {}
        }
    },
    methods: {
        openModal(component) {
            this.visible = true
            this.getLicenseList()
            this.component = component;

            this.$nextTick(() => {
                this.form.setFieldsValue({
                    classifier: this.component.classifier,
                    cpe: this.component.cpe,
                    //"expandDependencyGraph": this.component.expandDependencyGraph,
                    group: this.component.group,
                    // "isInternal": this.component.isInternal,
                    // "lastInheritedRiskScore": this.component.lastInheritedRiskScore,
                    license: this.component.license,
                    licenseUrl: this.component.licenseUrl,
                    name: this.component.name,
                    purl: this.component.purl,
                    author: this.component.author,
                    purlCoordinates: this.component.purlCoordinates,
                    usedBy: this.component.usedBy,
                    uuid: this.component.uuid,
                    version: this.component.version,
                    swid: this.component.swid,
                    filename: this.component.filename,
                    description: this.component.description,
                    spdx: this.component.spdx,
                    copyright: this.component.copyright,
                    md5: this.component.md5,
                    sha1: this.component.sha1,
                    sha256: this.component.sha256,
                    sha3256: this.component.sha3256,
                    sha3512: this.component.sha3512,
                    notes: this.component.notes,
                });
            })
        },
        handleOk() {
            this.form.validateFields((err, values) => {
                if (!err) {
                    this.loading = true
                    const params = {
                        ...values
                    }
                    updateComponent(params).then(res => {

                        this.$notification.success({
                            message: this.$t('Projects.UpdateSuccess'),
                            description: "",
                        })

                    }).finally(() => {
                        this.loading = false
                        this.handleCancel()
                        this.visible = false
                        this.$emit('handel')

                    });
                }
            })

        },
        handleCancel() {
            this.visible = false
        },
        handleDelete() {

            deleteComponent(this.component.uuid).then(res => {
                this.$notification.success({
                    message: this.$t('Projects.UpdateSuccess'),
                    description: "",
                })
            }).finally(() => {
                this.loading = false
                this.handleCancel()
                this.visible = false
                this.$emit('regression')

            });
        },
        getLicenseList() {
            getLicenseConcise().then((res) => {
                this.licenseList = res.data
            })
        }
    }
}
</script>

<style scoped lang="scss">
::v-deep .pro-tabs {
    .ant-tabs-nav-wrap {
        padding: 0;
    }

    .tab-pane {
        max-height: 70vh;
        overflow-y: scroll;
    }
}
</style>