<template>
    <a-modal
        :title="$t('Projects.CreateProperty')"
        :visible="visible"
        :width="1000"
        @cancel="handleCancel"
    >
        <a-form :form="form">
            <a-form-item class="by-m-b-0" :label="$t('Projects.GroupName')">
                <a-input
                    :placeholder="$t('Projects.GroupName')"
                    v-decorator="['groupName', { rules: [{ required: true, message: $t('Projects.GroupNameRequired') }] }]"
                />
            </a-form-item>
            <a-form-item class="by-m-b-0" :label="$t('Projects.PropertyName')">
                <a-input
                    :placeholder="$t('Projects.PropertyName')"
                    v-decorator="['propertyName', { rules: [{ required: true, message: $t('Projects.PropertyNameRequired') }] }]"
                />
            </a-form-item>
            <a-form-item class="by-m-b-0" :label="$t('Projects.PropertyValue')">
                <a-input
                    type="textarea"
                    :placeholder="$t('Projects.PropertyValue')"
                    v-decorator="['propertyValue', { rules: [{ required: true, message: $t('Projects.PropertyValueRequired') }] }]"
                />
            </a-form-item>
            <a-form-item class="by-m-b-0" :label="$t('Projects.PropertyType')">
<!--                <a-input-->
<!--                    :placeholder="$t('Projects.PropertyType')"-->
<!--                    v-decorator="['propertyType', { rules: [{ required: true, message: $t('Projects.PropertyTypeRequired') }] }]"-->
<!--                />-->
                <a-select
                    v-decorator="['propertyType', { rules: [{ required: true, message: $t('Projects.PropertyTypeRequired') }] }]"
                    :placeholder="$t('Projects.PropertyType')"
                >
                    <a-select-option
                        v-for="item in propertyTypeList"
                        :key="item.value"
                        :value="item.value"
                    >
                        {{ item.label }}
                    </a-select-option>
                </a-select>

            </a-form-item>
            <a-form-item class="by-m-b-0" :label="$t('Projects.Description')">
                <a-input
                    type="textarea"
                    :placeholder="$t('Projects.Description')"
                    v-decorator="['description']"
                />
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
import {addProjectProperty} from "@/api/projects";

export default {
    name: "CreateProperty",
    data() {
        return {
            visible: false,
            loading: false,
            form: this.$form.createForm(this, { name: 'create_property' }),
            projectUuid: '',
            propertyTypeList: [
                {
                    label: 'BOOLEAN',
                    value: 'BOOLEAN'
                },
                {
                    label: 'INTEGER',
                    value: 'INTEGER'
                },
                {
                    label: 'NUMBER',
                    value: 'NUMBER'
                },
                {
                    label: 'STRING',
                    value: 'STRING'
                },
                {
                    label: 'ENCRYPTEDSTRING',
                    value: 'ENCRYPTEDSTRING'
                },
                {
                    label: 'TIMESTAMP',
                    value: 'TIMESTAMP'
                },
                {
                    label: 'URL',
                    value: 'URL'
                },
                {
                    label: 'UUID',
                    value: 'UUID'
                },
            ],
        }
    },
    methods: {
        openModal(uuid) {
            this.visible = true
            this.projectUuid=uuid;
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
                    console.log("params", params);
                    addProjectProperty(this.projectUuid,params).then((res) => {
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