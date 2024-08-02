<template>
    <a-drawer
        placement="right"
        width="65%"
        :title="(isEdit ? $t('Groups.Edit') : $t('Groups.Create'))"
        :visible="visible"
        @close="closeModal"
    >
        <a-divider orientation="left">
            {{ $t('Groups.GroupSettings') }}
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
                        @blur="() => { $refs.name.onFieldBlur() }"
                    />
                </a-form-model-item>
                <a-form-model-item ref="name" :label="$t('Groups.Description')" prop="description">
                    <a-input
                        v-model="form.description"
                    />
                </a-form-model-item>
                <a-form-model-item ref="name" :label="$t('Groups.ExternalID')" prop="externalID">
                    <a-input
                        v-model="form.externalID"
                    />
                </a-form-model-item>
            </a-form-model>
            <a-divider orientation="left">
                {{ $t('Groups.Roles') }}
            </a-divider>
            <a-checkbox v-model="admin">
                {{ $t('Groups.AdministerPlatform') }}
            </a-checkbox>
            <a-checkbox v-model="resources" :disabled="resourcesDisabled">
                {{ $t('Groups.ManageResources') }}
                <a-tooltip placement="topLeft" :title="$t('Groups.ManageResourcesDesc')">
                    <a-icon type="question-circle" />
                </a-tooltip>
            </a-checkbox>
            <a-divider orientation="left">
                {{ $t('Groups.Options') }}
            </a-divider>
            <a-checkbox v-model="auto" :disabled="autoDisabled">
                {{ $t('Groups.Automatically') }}
            </a-checkbox>
        </div>
        <a-divider orientation="left">
            {{ $t('Groups.Users') }}
        </a-divider>
        <a-transfer
            :data-source="mockData"
            :target-keys="targetKeys"
            :show-search="true"
            :filter-option="(inputValue, item) => item.title.indexOf(inputValue) !== -1"
            :show-select-all="false"
            @change="onChange"
        >
            <template
                slot="children"
                slot-scope="{
                    props: { direction, filteredItems, selectedKeys, disabled: listDisabled },
                    on: { itemSelectAll, itemSelect },
                }"
            >
                <a-table
                    :row-selection="getRowSelection({ disabled: listDisabled, selectedKeys, itemSelectAll, itemSelect })"
                    :columns="direction === 'left' ? leftTableColumns : rightTableColumns"
                    :data-source="filteredItems"
                    size="small"
                    :pagination="false"
                    :style="{ pointerEvents: listDisabled ? 'none' : null }"
                    :custom-row="
                        ({ key, disabled: itemDisabled }) => ({
                            on: {
                                click: () => {
                                    if (itemDisabled || listDisabled) return;
                                    itemSelect(key, !selectedKeys.includes(key));
                                },
                            },
                        })
                    "
                />
            </template>
        </a-transfer>
        <a-divider orientation="left">
            {{ $t('Groups.GroupPermissions') }}
        </a-divider>
        <a-tabs v-model="tabKey">
            <a-tab-pane key="repositories" :tab="$t('Groups.Repositories')"></a-tab-pane>
            <a-tab-pane key="builds" :tab="`${$t('Groups.Builds')}`"></a-tab-pane>
            <a-tab-pane key="bundles" :tab="`${$t('Groups.ReleaseBundles')}`"></a-tab-pane>
            <div slot="tabBarExtraContent">
                <a-input v-model="searchText" :placeholder="$t('Permissions.Search')" allow-clear size="small">
                    <a-icon slot="prefix" type="search" />
                </a-input>
            </div>
        </a-tabs>
        <a-table
            :columns="i18nPermissionColumns"
            :data-source="permissionsList"
            :row-key="(r, i) => i.toString()"
            :pagination="{ pageSize: limit, current: page, total: total, showLessItems: true }"
        >
            <div slot="responseStatus" slot-scope="responseStatus">
                <a-tag color="#f50" v-if="responseStatus != 200">
                    {{ $t('Setting.Error') }}
                </a-tag>
                <a-tag color="#87d068" v-else>
                    {{responseStatus}}
                </a-tag>
            </div>
        </a-table>
    </a-drawer>
</template>

<script>
import { difference } from "lodash";

export default {
    name: "modal",
    data() {
        return {
            visible: false,
            isEdit: false,
            form: {
                name: '',
                description: '',
                ExternalID: '',
            },
            labelCol: { span: 24 },
            wrapperCol: { span: 24 },
            admin: false,
            resources: false,
            resourcesDisabled: false,
            auto: false,
            autoDisabled: false,
            mockData: [
                {
                    key: '1',
                    title: 'User 1',
                },
            ],
            targetKeys: [],
            targetData: [],
            leftTableColumns: [
                {
                    dataIndex: 'title',
                    title: 'Name',
                },
            ],
            rightTableColumns: [
                {
                    dataIndex: 'title',
                    title: 'Name',
                },
            ],
            tabKey: 'repositories',
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
                    scopedSlots: { customRender: 'download' },
                },
                {
                    title: '部署/缓存',
                    i18nKey: 'Groups.DeployCache',
                    dataIndex: 'deployCache',
                    key: 'deployCache',
                    scopedSlots: { customRender: 'deployCache' },
                },
                {
                    title: '删除/更新',
                    i18nKey: 'Groups.DeleteUpdate',
                    dataIndex: 'deleteUpdate',
                    key: 'deleteUpdate',
                    scopedSlots: { customRender: 'deleteUpdate' },
                },
            ],
            sourceData: [
                {
                    name: 'test',
                    download: 'test',
                    deployCache: 'test',
                    deleteUpdate: 'test'
                }
            ]
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
        }
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
        openModal(isEdit) {
            this.visible = true;
            this.isEdit = isEdit;
        },
        closeModal() {
            this.visible = false;
        },
        onChange(nextTargetKeys) {
            this.targetKeys = nextTargetKeys;
            this.targetData = this.mockData.filter(
                item => nextTargetKeys.indexOf(item.key) !== -1
            );
        },
        getRowSelection({ disabled, selectedKeys, itemSelectAll, itemSelect }) {
            return {
                getCheckboxProps: item => ({ props: { disabled: disabled || item.disabled } }),
                onSelectAll(selected, selectedRows) {
                    const treeSelectedKeys = selectedRows
                        .filter(item => !item.disabled)
                        .map(({ key }) => key);
                    const diffKeys = selected
                        ? difference(treeSelectedKeys, selectedKeys)
                        : difference(selectedKeys, treeSelectedKeys);
                    itemSelectAll(diffKeys, selected);
                },
                onSelect({ key }, selected) {
                    itemSelect(key, selected);
                },
                selectedRowKeys: selectedKeys,
            };
        },
    }
}
</script>

<style scoped lang="scss">

</style>