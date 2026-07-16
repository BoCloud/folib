<template>
    <a-modal
        :title="$t('Projects.CreateVersion')"
        :visible="visible"
        :width="1000"
        @cancel="handleCancel"
    >
        <a-form :form="form">
            <a-form-item class="by-m-b-0" :label="$t('Projects.Version')">
                <a-input
                    :placeholder="$t('Projects.Version')"
                    v-decorator="['version', { rules: [{ required: true, message: $t('Projects.VersionRequired') }] }]"
                />
            </a-form-item>
            <a-form-item class="by-m-b-0">
                <a-switch
                    v-decorator="['includeTags']"
                    v-model="includeTags"
                >
                    <a-icon slot="checkedChildren" type="check" />
                    <a-icon slot="unCheckedChildren" type="close" />
                </a-switch>
                <span class="by-m-l-10">{{ $t('Projects.Includes') }}{{ $t('Projects.Tags') }}</span>
            </a-form-item>
            <a-form-item class="by-m-b-0">
                <a-switch
                    v-decorator="['includeProperties']"
                    v-model="includeProperties"
                >
                    <a-icon slot="checkedChildren" type="check" />
                    <a-icon slot="unCheckedChildren" type="close" />
                </a-switch>
                <span class="by-m-l-10">{{ $t('Projects.Includes') }}{{ $t('Projects.Property') }}</span>
            </a-form-item>
            <a-form-item class="by-m-b-0">
                <a-switch
                    v-decorator="['includeComponents']"
                    v-model="includeComponents"
                >
                    <a-icon slot="checkedChildren" type="check" />
                    <a-icon slot="unCheckedChildren" type="close" />
                </a-switch>
                <span class="by-m-l-10">{{ $t('Projects.Includes') }}{{ $t('Projects.Component') }}</span>
            </a-form-item>
            <a-form-item class="by-m-b-0">
                <a-switch
                    v-decorator="['includeServices']"
                    v-model="includeServices"
                >
                    <a-icon slot="checkedChildren" type="check" />
                    <a-icon slot="unCheckedChildren" type="close" />
                </a-switch>
                <span class="by-m-l-10">{{ $t('Projects.Includes') }}{{ $t('Projects.Services') }}</span>
            </a-form-item>
            <a-form-item class="by-m-b-0">
                <a-switch
                    v-decorator="['includeAuditHistory']"
                    v-model="includeAuditHistory"
                >
                    <a-icon slot="checkedChildren" type="check" />
                    <a-icon slot="unCheckedChildren" type="close" />
                </a-switch>
                <span class="by-m-l-10">{{ $t('Projects.Includes') }}{{ $t('Projects.AuditHistory') }}</span>
            </a-form-item>
            <a-form-item class="by-m-b-0">
                <a-switch
                    v-decorator="['includeACL']"
                    v-model="includeACL"
                >
                    <a-icon slot="checkedChildren" type="check" />
                    <a-icon slot="unCheckedChildren" type="close" />
                </a-switch>
                <span class="by-m-l-10">{{ $t('Projects.Includes') }}{{ $t('Projects.AccessControlList') }}</span>
            </a-form-item>

        </a-form>
        <template slot="footer">
            <a-button key="back" @click="handleCancel">
                {{ $t(`Projects.Cancel`) }}
            </a-button>
            <a-button key="submit" type="primary" :loading="loading" @click="handleOK">
                {{ $t(`Projects.Create`) }}
            </a-button>
        </template>
    </a-modal>
</template>
<script>
import {cloneProject} from "@/api/projects";

export default {
    name: "CreateVersion",
    data() {
        return {
            visible: false,
            loading: false,
            form: this.$form.createForm(this, {name: 'create_version'}),
            projectData: {},
            includeACL: true,
            includeAuditHistory: true,
            includeComponents: true,
            includeProperties: true,
            includeServices: true,
            includeTags: true
        }
    },
    methods: {
        openModal(data) {
            this.visible = true;
            this.projectData = data;
        },
        handleCancel() {
            this.visible = false
        },
        handleOK() {
            this.form.validateFields((errors, values) => {
                if (!errors) {
                    this.loading = true
                    const params = {
                        ...values
                    }
                    // console.log("params", params);
                    // console.log("projectData",     this.projectData);
                    const data = {
                        "project": this.projectData.uuid,
                        "version": params.version,
                        "includeTags": this.includeTags,
                        "includeProperties": this.includeProperties,
                        "includeComponents": this.includeComponents,
                        "includeServices": this.includeServices,
                        "includeAuditHistory": this.includeAuditHistory,
                        "includeACL": this.includeACL
                    }
                    cloneProject(data).then((res) => {
                        if (res.status === 200) {
                            this.$notification.success({
                                message: this.$t('Projects.UpdateMessage'),
                                description: "",
                            });
                            this.$emit('handle')
                            this.handleCancel();
                        }
                    }).catch(error => {
                        this.$notification.error({
                            message: this.$t("操作失败"),
                            description: "",
                        });
                    }).finally(() => {
                        this.loading = false
                    });
                }
            })
        }
    }
}
</script>

<style scoped lang="scss">

</style>