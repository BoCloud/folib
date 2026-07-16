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
                <a-tab-pane key="1" :tab="$t('Projects.DetailGeneral')" class="tab-pane">
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.ProjectName') }}
                            <a-tooltip :title="$t('Projects.ProNameTip')">
                                <a-icon type="info-circle" />
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.NameRequired')"
                            v-decorator="['name', { rules: [{ required: true, message: $t('Projects.NameRequired') }] }]"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.Version') }}
                            <a-tooltip :title="$t('Projects.VersionTip')">
                                <a-icon type="info-circle" />
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.VersionRequired')"
                            v-model="version"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.Classifier') }}
                            <a-tooltip :title="$t('Projects.ClassifierTip')">
                                <a-icon type="info-circle" />
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
                    <a-form-item class="by-m-b-0" :label="$t('Projects.Parent')">
<!--                        <a-select-->
<!--                            :placeholder="$t('Projects.ParentPlaceholder')"-->
<!--                            v-decorator="['parent']"-->
<!--                        >-->
<!--                            <a-select-option-->
<!--                                v-for="item in parentList"-->
<!--                                :key="item.value"-->
<!--                                :value="item.value"-->
<!--                            >-->
<!--                                {{ item.label }}-->
<!--                            </a-select-option>-->
<!--                        </a-select>-->
                        <a-select property="$t('Projects.Parent')"
                                  v-model="parent"
                                  show-search
                                  :default-active-first-option="false" :filter-option="false"
                                  :placeholder="$t('Projects.ParentPlaceholder')"
                                  @search="handleParentChange">
                            <a-select-option v-for="(item, index) in parents" :key="item.uuid"
                                             :value="item.uuid">
                                             {{ item.name }}
                            </a-select-option>
                        </a-select>
                    </a-form-item>
                    <a-form-item class="by-m-b-0" :label="$t('Projects.Description')">
                        <a-input
                            type="textarea"
                            v-decorator="['description']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0" :label="$t('Projects.Tags')">
                        <a-select
                            mode="tags"
                            v-decorator="['tags']"
                        >
                        </a-select>
                    </a-form-item>
                    <a-form-item class="by-m-b-0 by-m-t-10">
                        <a-switch
                            v-decorator="['active']"
                            v-model="isActive"
                        >
                            <a-icon slot="checkedChildren" type="check" />
                            <a-icon slot="unCheckedChildren" type="close" />
                        </a-switch>
                        <span class="by-m-l-10">{{ $t('Projects.Active') }}</span>
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.ObjectIdentifier') }}
                            <a-tooltip :title="$t('Projects.ObjectIdentifierTip')">
                                <a-icon type="info-circle" />
                            </a-tooltip>
                        </span>
                        <a-input
                            disabled
                            v-decorator="['uuid', { rules: [{ required: true, message: $t('Projects.NameRequired') }] }]"
                        />
                    </a-form-item>
                </a-tab-pane>
                <a-tab-pane key="2" :tab="$t('Projects.Identity')">
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.ProjectName') }}
                            <a-tooltip :title="$t('Projects.ProNameTip')">
                                <a-icon type="info-circle" />
                            </a-tooltip>
                        </span>
                        <a-input
                            disabled
                            v-decorator="['name']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.Version') }}
                            <a-tooltip :title="$t('Projects.VersionTip')">
                                <a-icon type="info-circle" />
                            </a-tooltip>
                        </span>
                        <a-input
                            disabled
                            v-decorator="['version']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.NGV') }}
                            <a-tooltip :title="$t('Projects.NGVTip')">
                                <a-icon type="info-circle" />
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.NGV')"
                            v-decorator="['group']"
                        />
                    </a-form-item>
                    <a-form-item class="by-m-b-0">
                        <span slot="label">
                            {{ $t('Projects.PURL') }}
                            <a-tooltip :title="$t('Projects.PURLTip')">
                                <a-icon type="info-circle" />
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
                                <a-icon type="info-circle" />
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
                                <a-icon type="info-circle" />
                            </a-tooltip>
                        </span>
                        <a-input
                            :placeholder="$t('Projects.SWID')"
                            v-decorator="['swidTagId']"
                        />
                    </a-form-item>
                </a-tab-pane>
                <a-tab-pane key="3" :tab="$t('Projects.ExternalReferences')">
                    <a-table :columns="columns" :data-source="tableData" rowKey="uuid"></a-table>
                </a-tab-pane>
            </a-tabs>
        </a-form>
        <template slot="footer">
            <!-- <a-button key="delete" type="danger" ghost @click="handleDelete">
                {{ $t(`Projects.Delete`) }}
            </a-button>
            <a-button key="property" type="primary" ghost @click="openProperty">
                {{ $t(`Projects.Property`) }}
            </a-button>
            <a-button key="version" type="primary" ghost @click="AddVersion">
                {{ $t(`Projects.AddVersion`) }}
            </a-button>  -->
            <a-button key="back" @click="handleCancel">
                {{ $t(`Projects.Cancel`) }}
            </a-button>
            <!-- <a-button key="submit" type="primary" :loading="loading" @click="handleOk">
                {{ $t(`Projects.Update`) }}
            </a-button> -->
        </template>
        <project-properties ref="properties"></project-properties>
        <create-version @handle="handleRefresh" ref="version"></create-version>
    </a-modal>
</template>
<script>
import ProjectProperties from "./ProjectProperties.vue";
import CreateVersion from "./CreateVersion.vue";
import {addViolationAnalysis, deleteProject, getProjectsList, updateProject} from "@/api/projects";
import handle from "ant-design-vue/lib/vc-slider/src/Handle";
export default {
    name: "ProjectDetailModal",
    computed: {
        handle() {
            return handle
        }
    },
    components: {
        ProjectProperties,
        CreateVersion
    },
    data() {
        return {
            visible: false,
            form: this.$form.createForm(this, { name: 'pro_detail' }),
            classifierList: [
                {
                    label: 'Application',
                    value: '1'
                },
                {
                    label: 'Framework',
                    value: '2'
                },
                {
                    label: 'Library',
                    value: '3'
                },
                {
                    label: 'Container',
                    value: '4'
                },
                {
                    label: 'Operating system',
                    value: '5'
                },
                {
                    label: 'Device',
                    value: '6'
                },
                {
                    label: 'Firmware',
                    value: '7'
                },
                {
                    label: 'File',
                    value: '8'
                },
            ],
            parentList: [],
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
            projectData:{},
            isActive: false,
            parents:[],
            parent: null,
            version: null,
        }
    },
    methods: {
        openModal(data) {
            this.visible = true
            this.projectData = data;
            this.isActive = this.projectData.active;
            this.parent = this.projectData.parent ? this.projectData.parent.uuid : null;
            this.version = this.projectData.version;
            this.handleParentChange()
            this.$nextTick(() => {
                this.form.setFieldsValue({
                    active: true,
                    classifier: this.projectData.classifier,
                    name: this.projectData.name,
                    uuid: this.projectData.uuid,
                    parent: this.parent,
                    version: this.projectData.version,
                    description: this.projectData.description
                });
            })
        },
        handleParentChange(value){
            getProjectsList({searchText: value, excludeInactive: true}).then((res) => {
                this.parents = res.data
            })
        },
        handleOk() {
            this.form.validateFields((err, values) => {
                if (!err) {
                    this.loading = true
                    const params = {
                        ...values
                    }
                    const data ={
                        "active": this.isActive,
                        "classifier": params.classifier,
                        "name": params.name,
                        "uuid": params.uuid,
                        "parent": {
                            uuid: this.parent,
                        },
                        "version": this.version,
                        "description": params.description
                    }
                    updateProject(data).then((res) => {
                        if (res.status === 200) {
                            this.$notification.success({
                                message: this.$t('Projects.UpdateMessage'),
                                description: "",
                            });
                            this.$emit('handle')
                            this.handleCancel();
                        }
                    }).finally(() => {
                        this.loading = false
                    });
                }
            });
        },
        handleCancel() {
            this.visible = false
        },
        AddVersion() {
            this.$refs.version.openModal(this.projectData)
        },
        openProperty() {
            this.$refs.properties.openModal(this.projectData.uuid)
        },
        handleDelete() {
            //console.log("uuid",this.projectData)
            deleteProject(this.projectData.uuid).then((res) => {
                if (res.status === 204) {
                    this.$notification.success({
                        message: this.$t('Projects.DeleteSuccess'),
                        description: "",
                    });
                    this.handleCancel();
                    this.$router.push(`/projects`)
                }
            }).catch(error => {
                this.$notification.error({
                    message: this.$t("操作失败"),
                    description: "",
                });
            }).finally(() => {
            });
        },
        handleRefresh(){
            this.$emit('handle')
            this.handleCancel();
        }
    }
}
</script>

<style scoped lang="scss">
::v-deep .pro-tabs  {
    .ant-tabs-nav-wrap {
        padding: 0;
    }

    .tab-pane {
        max-height: 70vh;
        overflow-y: scroll;
    }
}
</style>