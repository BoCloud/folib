<template>
    <a-drawer
        placement="right"
        width="65%"
        :title="(isView ? $t('Groups.View') : isEdit ? $t('Groups.Edit') : $t('Groups.Create'))"
        :visible="visible"
        @close="closeModal"
    >
        <a-spin :spinning="spinning">
            <div class="by-p-b-60">
                <a-divider orientation="left">
                   <span class="by-f-w-600"> {{ $t('Groups.GroupSettings') }} </span>
                </a-divider>
                <div class="by-p-l-60">
                    <a-form-model
                        ref="ruleForm"
                        layout="inline"
                        :model="form"
                        :rules="rules"
                        :label-col="labelCol"
                        :wrapper-col="wrapperCol"

                    >
                        <a-form-model-item ref="name" :label="$t('Groups.GroupName')" prop="name">
                            <a-input
                                v-model="form.name"
                                :disabled="isView"
                                @blur="() => { $refs.name.onFieldBlur() }"
                            />
                        </a-form-model-item>
                        <a-form-model-item ref="name" :label="$t('Groups.Description')" prop="description">
                            <a-input
                                v-model="form.description"
                                :disabled="isView"
                            />
                        </a-form-model-item>
                    </a-form-model>
                    <!-- <a-divider orientation="left">
                        {{ $t('Groups.Options') }}
                    </a-divider>-->
                    <a-checkbox v-model="auto" :disabled="isView" class="by-m-t-10">
                        {{ $t('Groups.Automatically') }}
                    </a-checkbox>
                </div>
                <a-divider orientation="left">
                    <span class="by-f-w-600">{{ $t('Groups.Users') }}</span>
                </a-divider>
                <div class="by-flex by-row-right by-m-b-10">
                    <a-input-search v-model="userSearchText" size="small" :placeholder="$t('Groups.EnterTheNameQuery')" @search="handleSearch()" class="by-w-200"/>
                </div>
                <a-table
                    :row-selection="{ selectedRowKeys: selectedRowKeys, onChange: onSelectChange, getCheckboxProps: getCheckboxProps }"
                    :columns="tableColumns"
                    :data-source="tableData"
                    :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true }"
                    :loading="loading"
                    @change="handleChangeTable"
                    :scroll="{ y: 500 }"
                />
                <template v-if="isEdit">
                    <a-divider orientation="left">
                        <span class="by-f-w-600">{{ $t('Groups.GroupPermissions') }}</span>
                    </a-divider>
                    <a-table
                        :columns="i18nPermissionColumns"
                        :data-source="permissionsList"
                        :row-key="(r, i) => i.toString()"
                        :pagination="false"
                        :scroll="{ y: 300 }"
                    >
                        <div slot="customTag" slot-scope="status">
                            <div class="join-status" v-if="status">
                                <a-icon type="check" />
                            </div>
                        </div>
                    </a-table>
                </template>
            </div>
        </a-spin>
        <div v-if="!isView" class="drawer-footer">
            <a-button :style="{ marginRight: '8px' }" @click="closeModal">
                {{ $t(`Permissions.Cancel`) }}
            </a-button>
            <a-button type="primary" :loading="confirmLoading" @click="handleConfirm">
                {{ $t(`Permissions.Confirm`) }}
            </a-button>
        </div>
    </a-drawer>
</template>

<script>
import { queryUser } from "@/api/users";
import { getGroupDetail, createGroup, updateGroup } from "@/api/group";

export default {
    name: "modal",
    data() {
        return {
            visible: false,
            spinning: false,
            isView: false,
            isEdit: false,
            loading: false,
            confirmLoading: false,
            form: {
                id: '',
                name: '',
                description: '',
            },
            labelCol: { span: 24 },
            wrapperCol: { span: 24 },
            admin: false,
            resources: false,
            resourcesDisabled: false,
            auto: true,
            autoDisabled: false,
            page: 1,
            limit: 10,
            total: 0,
            tableData: [],
            selectedRowKeys: [],
            userSearchText: '',
            searchText: '',
            permissionsColumns: [
                {
                    title: '权限名称',
                    i18nKey: 'Groups.PermissionsName',
                    dataIndex: 'name',
                    key: 'name',
                    width: 200,
                    scopedSlots: { customRender: 'name' },
                },
                {
                    title: '下载',
                    i18nKey: 'Groups.Download',
                    dataIndex: 'download',
                    key: 'download',
                    scopedSlots: { customRender: 'customTag' },
                },
                {
                    title: '部署/缓存',
                    i18nKey: 'Groups.DeployCache',
                    dataIndex: 'deployCache',
                    key: 'deployCache',
                    scopedSlots: { customRender: 'customTag' },
                },
                {
                    title: '删除/更新',
                    i18nKey: 'Groups.DeleteUpdate',
                    dataIndex: 'deleteUpdate',
                    key: 'deleteUpdate',
                    scopedSlots: { customRender: 'customTag' },
                },
                {
                    title: '晋级/分发',
                    i18nKey: 'Groups.PromoDistribution',
                    dataIndex: 'promoDistribution',
                    key: 'promoDistribution',
                    scopedSlots: { customRender: 'customTag' },
                },
            ],
            sourceData: []
        }
    },
    computed: {
        rules() {
            return {
                name: [
                    { required: true, message: this.$t('Groups.NameRequired'), trigger: 'blur' },
                    // { min: 3, max: 20, message: this.$t('Groups.GroupName'), trigger: 'blur' }
                ]
            }
        },
        i18nPermissionColumns () {
            return this.permissionsColumns.map(column => {
                if (column.i18nKey) {
                    column.title = this.$t(column.i18nKey);
                }
                return column;
            })
        },
        permissionsList() {
            return this.sourceData.filter(item => {
                return item.name.indexOf(this.searchText) !== -1;
            })
        },
        tableColumns() {
            return [ { dataIndex: 'title', title: this.$t('Permissions.SingleName') } ]
        },
    },
    watch: {
        admin(val) {
            if (val) {
                this.resources = true;
                this.auto = false;
                this.resourcesDisabled = true;
                this.autoDisabled = true;
            } else {
                this.resourcesDisabled = false;
                this.autoDisabled = false;
            }
        }
    },
    methods: {
        openModal(id, isView) {
            this.visible = true;
            this.isView = isView;
            this.isEdit = !!id;
            this.getUsers()
            if (id) this.getDetail(id)
        },
        closeModal() {
            this.page=1
            this.$refs.ruleForm.resetFields()
            this.auto = true
            this.selectedRowKeys = []
            this.visible = false;
        },
        onChange(nextTargetKeys) {
            this.targetKeys = nextTargetKeys;
            this.targetData = this.mockData.filter(
                item => nextTargetKeys.indexOf(item.key) !== -1
            );
        },
        getDetail(id)
        {
            this.spinning = true
            getGroupDetail(id).then(res => {
                const { roleAccess, userGroupDTO } = res
                this.form.id = id
                this.form.name = userGroupDTO.groupName
                this.form.description = userGroupDTO.description
                this.selectedRowKeys = userGroupDTO.userIds
                this.auto = userGroupDTO.joinGroup === '1'
                this.sourceData = []
                for (const key in roleAccess) {
                    this.sourceData.push({
                        name: key,
                        download: roleAccess[key].includes('ARTIFACTS_RESOLVE'),
                        deployCache: roleAccess[key].includes('ARTIFACTS_DEPLOY'),
                        promoDistribution: roleAccess[key].includes('ARTIFACTS_PROMOTION'),
                        deleteUpdate: roleAccess[key].includes('ARTIFACTS_DELETE')
                    })
                }
            }).finally(() => {
                this.spinning = false
            })
        },
        handleSearch() {
            this.page = 1
            this.getUsers()
        },
        handleChangeTable(pagination) {
            if (pagination) this.page = pagination.current
            this.getUsers()
        },
        getUsers()
        {
            this.loading = true
            queryUser({username: this.userSearchText}, {page: this.page, limit: this.limit}).then(res => {
                this.tableData = res.data.rows.map((item, index) => {
                    return {
                        key: item.username,
                        title: item.username,
                    }
                })
                this.total = res.data.total
            }).finally(() => {
                this.loading = false
            })
        },
        onSelectChange(selectedRowKeys) {
            this.selectedRowKeys = selectedRowKeys;
        },
        getCheckboxProps() {
            return {
                props: {
                    disabled: this.isView
                }
            }
        },
        handleConfirm()
        {
            this.$refs.ruleForm.validate(validate => {
                if (validate) {
                    this.confirmLoading = true
                    const params = {
                        id: this.form.id,
                        groupName: this.form.name,
                        description: this.form.description,
                        userIds: this.selectedRowKeys,
                        joinGroup: this.auto ? '1' : '0'
                    }
                    const method = this.isEdit ? updateGroup : createGroup;
                    method(params).then(res => {
                        this.$emit('reset')
                        this.closeModal()
                    }).finally(() => {
                        this.confirmLoading = false
                    })
                }
            })
        }
    }
}
</script>

<style scoped lang="scss">

</style>