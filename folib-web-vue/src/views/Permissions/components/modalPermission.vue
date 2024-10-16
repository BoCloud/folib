<template>
    <a-drawer placement="right" width="65%"
        :title="(isView ? $t('Permissions.View') : isEdit ? $t('Permissions.Edit') : $t('Permissions.Create'))"
        :visible="visible" @close="closeModal">
        <a-spin :spinning="spinning">
            <!-- 统计卡片 -->
            <a-row :gutter="16">
                <a-col class="gutter-row" :span="8">
                    <a-card :bordered="false" class="widget-1">
                        <div class="icon">
                            <a-icon type="database" />
                        </div>
                        <a-statistic :title="$t('Permissions.Resources')" :value="selectedResourcesSummary" :precision="0"
                            class="text-success">
                        </a-statistic>
                    </a-card>
                </a-col>
                <a-col class="gutter-row" :span="8">
                    <a-card :bordered="false" class="widget-1">
                        <div class="icon">
                            <a-icon type="user" />
                        </div>
                        <a-statistic :title="$t('Permissions.Users')" :value="selectedUsersSummary" :precision="0"
                            class="text-success">
                        </a-statistic>
                    </a-card>
                </a-col>
                <a-col class="gutter-row" :span="8">
                    <a-card :bordered="false" class="widget-1">
                        <div class="icon">
                            <a-icon type="team" />
                        </div>
                        <a-statistic :title="$t('Permissions.Groups')" :value="selectedGroupsSummary" :precision="0"
                            class="text-success">
                            <template #prefix></template>
                        </a-statistic>
                    </a-card>
                </a-col>
            </a-row>
            <!-- 标签页卡片 -->
            <a-card class="tabs-card widget-1 by-m-t-16" :bordered="false">
                <a-form-model ref="form" :model="form" :label-col="{ span: 6 }" :wrapper-col="{ span: 14 }">
                    <a-row :gutter="16">
                        <a-col :span="12">
                            <a-form-model-item prop="name" :label="$t('Permissions.Name')" :rules="[
                                { required: true, message: $t('Permissions.EnterTheNameCreate'), trigger: 'blur' },
                                { pattern: /^[0-9A-Za-z_|-]+$/, message: $t('Permissions.EnterTheNamePattern') }]">
                                <a-input v-model="form.name" :placeholder="$t('Permissions.EnterTheNameCreate')"
                                    :disabled="isView || isEdit" :maxLength="100" />
                            </a-form-model-item>
                        </a-col>
                        <a-col :span="12">
                            <a-form-model-item prop="description" :label="$t('Permissions.Description')">
                                <a-input v-model="form.description" :placeholder="$t('Permissions.EnterTheDescription')"
                                    :disabled="isView" :maxLength="500" />
                            </a-form-model-item>
                        </a-col>
                    </a-row>
                </a-form-model>
                <a-tabs class="custom-tabs by-m-t-16" v-model:activeKey="activeTab">
                    <a-tab-pane key="1" :tab="$t('Permissions.Resources')">
                        <div>
                            <a-button v-if="!isView && !isAdmin && !isStorageAdmin && !isStorageUser"
                                @click="showResourceModal" type="primary" class="by-m-b-10">
                                {{ $t('Permissions.SelectResources') }}
                            </a-button>
                            <a-table :columns="selectedResourcesColumns" :dataSource="selectedResources" :pagination="false"
                                size="small" class="selected-resources-table" rowKey="record => record.title">
                                <template slot="type" slot-scope="text">
                                    <span v-if="text === 'storage'">{{ $t('Permissions.StorageSpace') }}</span>
                                    <span v-else-if="text === 'repository'">{{ $t('Permissions.Repository') }}</span>
                                    <span v-else-if="text === 'path'">{{ $t('Permissions.Path') }}</span>
                                </template>
                            </a-table>
                        </div>
                        <a-card  class="by-m-t-10 anonymous-permissions-group" v-if="isAnonymous">
                            <span class="by-m-r-40"> {{ $t('Permissions.SelectedPermissions') }}</span>   
                            <a-checkbox-group v-model="anonymousPermissions" >
                                <a-checkbox v-for="option in permissionOptions" :key="option.value" :value="option.value">
                                    {{ option.label }}
                                </a-checkbox>
                            </a-checkbox-group>
                        </a-card>
                    </a-tab-pane>
                    <a-tab-pane key="2" :tab="$t('Permissions.Users')" :disabled="isAnonymous">
                        <a-row>
                            <a-col :span="17" class="transfer-container">
                                <a-transfer :data-source="allUsers" :target-keys="selectedUserKeys"
                                    :disabled="isView || isAnonymous" :render="item => item.title"
                                    @change="handleUserTransferChange" @scroll="handleScroll"
                                    @selectChange="handleUserSelectChange" :list-style="{
                                        width: '270px',
                                        height: '400px',
                                    }" :titles="[$t('Groups.transfer.titles[0]'), $t('Groups.transfer.titles[1]')]"
                                    :locale="{
                                        itemUnit: $t('Groups.transfer.itemUnit'),
                                        itemsUnit: $t('Groups.transfer.itemsUnit'),
                                        searchPlaceholder: $t('Groups.transfer.searchPlaceholder'),
                                    }" :show-search="true" :filter-option="handleFilter">

                                    <template slot="children" slot-scope="{
                                        props: { direction, filteredItems, selectedKeys, disabled: listDisabled },
                                        on: { itemSelectAll, itemSelect },
                                    }">
                                        <a-table v-if="direction === 'right'" :pagination="false" :scroll="{ y: 300 }"
                                            :show-header="false" :columns="userRightColumns" :data-source="filteredItems"
                                            :row-key="record => record.title"
                                            :row-selection="getUserRowSelection({ selectedKeys, itemSelectAll, itemSelect })"
                                            size="small" class="custom-right-table">
                                            <template slot="permissions" slot-scope="text, record, i">
                                                <template
                                                    v-if="(!userPermissions[record.title] || userPermissions[record.title].length === 0) && !isAdmin">
                                                    <a-tooltip :title="$t('Permissions.NoPermissionsTooltip')">
                                                        <a-icon type="question-circle" />
                                                    </a-tooltip>
                                                </template>
                                            </template>
                                        </a-table>
                                    </template>
                                </a-transfer>
                            </a-col>
                            <a-col :span="7">
                                <div class="permissions-checkbox-container">
                                    <div class="permission-title">
                                        <a-checkbox :indeterminate="indeterminate" :checked="checkAll"
                                            :disabled="isView || isAdmin ||isStorageAdmin" @change="onCheckAllChange">
                                            {{ $t('Permissions.SelectedPermissions') }}
                                        </a-checkbox>
                                 </div>
                                    <a-checkbox-group v-model="selectedUserPermissions" @change="handleUserPermissionChange"
                                        style="display: flex; flex-direction: column;">
                                        <a-checkbox v-for="option in permissionOptions" :key="option.value"
                                            :disabled="isView || isAdmin" :value="option.value">
                                            {{ option.label }}
                                        </a-checkbox>
                                    </a-checkbox-group>
                                </div>
                            </a-col>
                        </a-row>
                    </a-tab-pane>
                    <a-tab-pane key="3" :tab="$t('Permissions.Groups')" :disabled="isAnonymous||isStorageAdmin">
                        <a-row>
                            <a-col :span="17">
                                <a-transfer :data-source="allGroups" :target-keys="selectedGroupKeys"
                                    :disabled="isView || isAnonymous" :render="item => item.title"
                                    @change="handleGroupTransferChange" @selectChange="handleGroupSelectChange" :list-style="{
                                        width: '270px',
                                        height: '400px',
                                    }"
                                    :titles="[$t('Groups.transfer.groupTitles[0]'), $t('Groups.transfer.groupTitles[1]')]"
                                    :locale="{
                                        itemUnit: $t('Groups.transfer.groupItemUnit'),
                                        itemsUnit: $t('Groups.transfer.groupItemsUnit'),
                                        searchPlaceholder: $t('Groups.transfer.searchPlaceholder'),
                                    }" :show-search="true" :filter-option="handleGroupFilter"
                                    @scroll="handleGroupScroll" :row-key="record => record.id">
                                    <template slot="children" slot-scope="{
                                        props: { direction, filteredItems, selectedKeys, disabled: listDisabled },
                                        on: { itemSelectAll, itemSelect },
                                    }">
                                        <a-table v-if="direction === 'right'" :pagination="false" :scroll="{ y: 300 }"
                                            :show-header="false" :columns="groupRightColumns" :data-source="filteredItems"
                                            :row-key="record => record.id"
                                            :row-selection="getGroupRowSelection({ selectedKeys, itemSelectAll, itemSelect })"
                                            size="small" class="custom-right-table">
                                            <template slot="permissions" slot-scope="text, record, i">
                                                <template
                                                    v-if="(!groupPermissions[record.id] || groupPermissions[record.id].length === 0) && !isAdmin">
                                                    <a-tooltip :title="$t('Permissions.NoPermissionsTooltip')">
                                                        <a-icon type="question-circle" />
                                                    </a-tooltip>
                                                </template>
                                            </template>
                                        </a-table>
                                    </template>
                                </a-transfer>
                            </a-col>
                            <a-col :span="7">
                                <div class="permissions-checkbox-container">
                                    <div class="permission-title">
                                        <a-checkbox :indeterminate="groupIndeterminate" :checked="groupCheckAll"
                                            @change="onGroupCheckAllChange">
                                            {{ $t('Permissions.SelectedPermissions') }}
                                        </a-checkbox>
                                    </div>
                                    <a-checkbox-group v-model="selectedGroupPermissions"
                                        @change="handleGroupPermissionChange"
                                        style="display: flex; flex-direction: column;">
                                        <a-checkbox v-for="option in permissionOptions" :key="option.value"
                                            :disabled="isView || isAdmin" :value="option.value">
                                            {{ option.label }}
                                        </a-checkbox>
                                    </a-checkbox-group>
                                </div>
                            </a-col>
                        </a-row>
                    </a-tab-pane>
                </a-tabs>
            </a-card>

        </a-spin>
        <div class="drawer-footer" v-if="!isView">
            <a-button :style="{ marginRight: '8px' }" @click="closeModal">
                {{ $t(`Permissions.Cancel`) }}
            </a-button>
            <a-button type="primary" :loading="confirmLoading" @click="handleConfirm">
                {{ $t(`Permissions.Confirm`) }}
            </a-button>
        </div>
        <selectUserGroup ref="selectUserGroup" @confirm="selectUserGroupChange"></selectUserGroup>

        <!-- 资源选择弹框 -->
        <a-modal :visible="resourceModalVisible" :title="$t('Permissions.SelectResources')" @ok="handleResourceModalOk"
            @cancel="handleResourceModalCancel" width="61%" class="resource-modal">
            <a-table :columns="storageColumns" :dataSource="storageList" :rowSelection="{
                selectedRowKeys: selectedStorageKeys,
                onChange: onStorageSelectChange,
                type: 'checkbox'
            }" :pagination="{ pageSize: 10, total: storageTotal,current:storagePage}" @change="handleTableChange"
                :expandedRowKeys="expandedRowKeys" @expand="onExpand" :rowKey="(record) => record.id"
                :expandRowByClick="true">
                <template slot="expandIcon" slot-scope="props">
                    <a-icon :type="props.expanded ? 'minus-square' : 'plus-square'"
                        @click="e => props.onExpand(props.record, e)"
                        style="font-size: 16px; color: grey;  cursor: pointer;" />
                </template>
                <template slot="expandedRowRender" slot-scope="record">
                    <div v-if="record.repositories && record.repositories.length" class="expanded-row-content">
                        <div class="repository-search">

                        </div>
                        <a-table class="repository-table" :columns="perRepositoryColumns"
                            :data-source="filteredRepositories(record)" :row-selection="{
                                selectedRowKeys: selectedRepositoryKeys,
                                onChange: onRepositoriesChange,
                                type: 'checkbox',
                                columnWidth: 30
                            }" :row-key="(record) => record.id" :pagination="false" :scroll="{ y: 200 }">
                            <div slot="includes" slot-scope="text,record">
                                <div class="insert-item" :class="{ 'has-error': record.isInError }">
                                    <a-input v-model="record.currentInPattern" :placeholder="$t('Permissions.NewPatterns')"
                                        @focus="record.isInError = false" @blur="handleAddRowInPattern(record)" />
                                    <a-icon type="plus" class="per-plus-icon by-m-l-10"
                                        @click="handleAddRowInPattern(record)" />
                                </div>
                                <div v-if="record.isInError" class="err-tip">{{
                                    $t('Permissions.ErrPatterns') }}
                                </div>
                                <div class="insert-content per-insert-content">
                                    <div v-for="(item, index) in record.includes" :key="index"
                                        class="by-m-t-10 single-pattern-item">
                                        {{ item }}
                                        <a-icon type="close" class="close-icon ml-4" @click="removePath(record, index)" />
                                    </div>
                                </div>
                            </div>
                            <template slot="repositoriesTitle">
                                <div class="search-slot">
                                    <div>
                                        <span>{{ $t('Permissions.Repositories') }}</span>
                                        <a-tooltip :title="$t('Permissions.RepositoryTooltip')">
                                            <a-icon type="question-circle" style="margin-left: 4px;" />
                                        </a-tooltip>
                                    </div>
                                    <div class="search-container">
                                        <a-input v-model="record.searchQuery"
                                            :placeholder="$t('Permissions.SearchRepositories')" @change="onSearch(record)"
                                            :class="{ 'expanded': searchExpandedMap[record.id] }" />
                                        <a-icon type="search" class="search-icon" @click.stop="toggleSearch(record)" />
                                    </div>
                                </div>
                            </template>
                            <template slot="includesTitle">
                                <span>{{ $t('Permissions.IncludePatterns') }}</span>
                                <a-tooltip :title="$t('Permissions.IncludeTooltip')">
                                    <a-icon type="question-circle" style="margin-left: 4px;" />
                                </a-tooltip>

                            </template>
                        </a-table>
                    </div>
                    <div v-else-if="!record.repositories">
                        <a-spin />
                    </div>
                </template>
                <template slot="storageSpaceTitle">
                    <span>{{ $t('Permissions.StorageSpace') }}</span>
                    <a-tooltip :title="$t('Permissions.StorageSpaceTooltip')">
                        <a-icon type="question-circle" style="margin-left: 4px;" />
                    </a-tooltip>
                </template>
            </a-table>
        </a-modal>
    </a-drawer>
</template>

<script>
import repositories from "./repositories.vue";
import selectUserGroup from "./selectUserGroup.vue";
import { queryUser } from "@/api/users";
import { getPermissionDetail, createPermission, getPermissionUsers, updatePermission } from "@/api/permissions";
import { queryStorages, queryRepositories, queryRepositoriesByStorage } from "@/api/folib";
import { getGroupList } from "@/api/group";
import TextOver from "@/components/Tools/textOver.vue";

export default {
    name: "modal",
    components: {
        TextOver,
        repositories,
        selectUserGroup
    },
    data() {
        return {
            visible: false,
            isView: false,
            isEdit: false,
            isAdmin: false,
            isStorageAdmin: false,
            isStorageUser: false,
            isAnonymous: false,
            confirmVisible: false,
            spinning: false,
            step: 0,
            form: {
                name: '',
                description: '',
            },
            repositoriesList: [],
            storageList: [],
            userSearch: '',
            userSelectList: [],
            userSelectCopyList: [],
            userAuthMap: {},
            currentUserIndex: 0,
            repositoriesCheckedList: [],
            repositoriesCheckAll: false,
            groupSelectList: [],
            groupSelectCopyList: [],
            groupAuthMap: {},
            currentGroupIndex: 0,
            groupCheckedList: [],
            groupCheckAll: false,
            confirmLoading: false,
            logoValueMap: [],
            selectedResources: [],
            repositoriesTotal: 0,
            storageTotal: 0,
            activeTab: "1", // 默认显示的标签页
            radioModel: 'StorageSpace',
            selectedRowKeys: [],
            expandedRowKeys: [],
            selectedStorageKeys: [],
            selectedRepositoryKeys: [],
            selectedPath: [],
            anonymousPermissions:[],
            storageColumns: [
                {
                    dataIndex: 'id',
                    key: 'id',
                    slots: { title: 'storageSpaceTitle' }
                }
            ],
            perRepositoryColumns: [
                {
                    dataIndex: 'title',
                    key: 'title',
                    width: '300px',
                    slots: { title: 'repositoriesTitle' }
                },
                {

                    dataIndex: 'includes',
                    key: 'includes',
                    slots: { title: 'includesTitle' },
                    scopedSlots: { customRender: 'includes' },
                },
            ],
            // 用户
            selectedRowKeys: [],
            userSearchText: '',
            searchText: '',
            allUsers: [],
            selectedUserKeys: [],
            userLoading: false,
            userPage: 1,
            userLimit: 20,
            userTotal: 0,
            userHasMore: true,
            userRightColumns: [
                {
                    title: this.$t('Permissions.User'),
                    dataIndex: 'title',
                    key: 'title',
                },
                {
                    dataIndex: 'permissions',
                    key: 'permissions',
                    width: '30%',
                    align: 'center',
                    scopedSlots: { customRender: 'permissions' }
                },
            ],
            // 右侧复选框选中的item
            selectedUserItems: [],
            // 用户组
            allGroups: [],
            selectedGroupKeys: [],
            groupLoading: false,
            groupPage: 1,
            groupLimit: 20,
            groupTotal: 0,
            groupHasMore: true,
            leftGroupSearchValue: '',
            searchExpandedMap: {},
            resourceModalVisible: false,
            selectedResourcesColumns: [
                {
                    title: this.$t('Permissions.ResourceName'),
                    dataIndex: 'title',
                    width: '50%',
                    key: 'title',
                },
                {
                    title: this.$t('Permissions.Type'),
                    dataIndex: 'type',
                    width: '10%',
                    key: 'type',
                    scopedSlots: { customRender: 'type' },
                },
                {
                    title: this.$t('Permissions.Path'),
                    dataIndex: 'path',
                    key: 'path',
                },
            ],
            groupRightColumns: [
                {
                    title: this.$t('Permissions.Group'),
                    dataIndex: 'title',
                    key: 'title',
                },
                {
                    dataIndex: 'permissions',
                    key: 'permissions',
                    width: '30%',
                    align: 'center',
                    scopedSlots: { customRender: 'permissions' }
                },
            ],
            selectedUserPermissions: [],
            // 用户组右侧被复选框选中的值
            selectedGroupItems: [],
            permissionOptions: [
                {
                    label: this.$t(`Permissions.Download`),
                    value: 'ARTIFACTS_RESOLVE',
                    enabled: false,
                    logo: 'download',
                    desc: this.$t(`Permissions.DownloadDesc`)
                },
                {
                    label: this.$t(`Permissions.DeployCache`),
                    value: 'ARTIFACTS_DEPLOY',
                    enabled: false,
                    logo: 'deployCache',
                    desc: this.$t(`Permissions.DeployCacheDesc`)
                },
                {
                    label: this.$t(`Permissions.DeleteUpdate`),
                    value: 'ARTIFACTS_DELETE',
                    enabled: false,
                    logo: 'deleteUpdate',
                    desc: this.$t(`Permissions.DeleteUpdateDesc`)
                },
            ],
            userPermissions: {},
            groupPermissions: {},
            currentSelectedUser: null,
            indeterminate: false,
            checkAll: false,
            selectedGroupPermissions: [],
            currentSelectedGroup: null,
            groupIndeterminate: false,
            storagePage:1,
        }
    },
    created() {
        this.getList();
    },
    computed: {

        selectedResourcesSummary() {
            return this.selectedResources.length;
        },
        selectedUsersSummary() {
            return this.selectedUserKeys.length;
        },
        selectedGroupsSummary() {
            return this.selectedGroupKeys.length;
        },
        selectedUsersList() {
            return this.selectedUserKeys.map(key => ({
                key: key,
                username: key
            }));
        },
        selectedGroupsList() {
            return this.selectedGroupKeys.map(key => {
                const group = this.allGroups.find(g => g.key === key);
                return {
                    key: key,
                    groupName: group ? group.title : key
                };
            });
        },
    },
    watch: {
        activeTab(newValue) {
            if (newValue === "2" && this.allUsers.length === 0) {
                this.getUsers();
            } else if (newValue === "3" && this.allGroups.length === 0) {
                this.getGroups();
            }
        }
    },

    methods: {
        async openModal(id, isView, isAdmin) {
            this.visible = true
            this.spinning = true
            this.isView = isView
            this.isAdmin = isAdmin
            this.isStorageAdmin = id?.startsWith('STORAGE_ADMIN_')
            this.isStorageUser = id?.startsWith('STORAGE_USER_')
            this.isAnonymous = id === 'ANONYMOUS'
            this.isEdit = !!id
            // this.init()
            await this.getStorageList()
            await this.getRepositoriesList()
            if (id) {
                if (this.isStorageAdmin || this.isAdmin || this.isStorageUser) {
                    this.activeTab = '2'
                } else {
                    this.activeTab = '1'
                }
                this.getDetail(id);
            } else {
                this.spinning = false
            }
        },
        closeModal() {
            this.visible = false;
            this.selectedResources = [],
            this.resetData;
            this.form = {};
            this.selectedUserKeys = [];
            this.selectedGroupKeys = [];
            this.selectedPath = [];
            this.userPermissions = {};
            this.groupPermissions = {};
            this.selectedRepositoryKeys = [];
            this.selectedStorageKeys = [];
            this.groupIndeterminate = false;
            this.groupCheckAll = false;
            this.selectedGroupPermissions = [];
            this.indeterminate = false;
            this.checkAll = false;
            this.selectedUserPermissions = [];
            this.allGroups=[],
            this.allUsers=[],
            this.storageList=[],
            this.storagePage=1,
            this.storageTotal=0,
            this.repositoriesList=[],
            this.repositoriesTotal=0,
            this.resetGroupData;
            this.activeTab = "1";
        },
        getDetail(id) {
            this.spinning = true;
            getPermissionDetail(id).then(res => {
                const { name, description, privileges, resources ,access} = res
                this.form.name = name;
                this.form.description = description;
                // 用户权限
                this.userPermissions = {}
                this.selectedUserKeys = []
                privileges.users.forEach(item => {
                    this.userPermissions[item.id] = item.access
                    this.selectedUserKeys.push(item.id)
                })
                // 用户组权限
                this.groupPermissions = {}
                this.selectedGroupKeys = []
                privileges.groups.forEach(item => {
                    this.groupPermissions[item.id] = item.access
                    this.selectedGroupKeys.push(item.id)
                })
                // 资源
                this.selectedResources = resources.map(item => {
                    const i = {}
                    i.title = item.resourceId
                    i.storageId = item.storageId;
                    if (item.path) {
                        i.type = 'path';
                        i.repositoryId = item.repositoryId;
                        i.path = item.path;

                        this.selectedPath.push({
                            storageId: item.storageId,
                            repositoryId: item.repositoryId,
                            path: item.path,
                        });

                    } else if (item.repositoryId) {
                        i.type = 'repository';
                        i.repositoryId = item.repositoryId;
                        const key = `${item.storageId}/${item.repositoryId}`
                        this.selectedRepositoryKeys.push(key);
                    } else {
                        i.type = 'storage';
                        this.selectedStorageKeys.push(item.storageId);
                    }
                    return i;
                });
                // 加载用户和用户组数据
                if(this.isAnonymous){
                    this.anonymousPermissions = access || [];     
                }else{
                    this.getUsers();
                    this.getGroups();
                }    
            }).finally(() => {
                this.spinning = false;
            })
        },
        getUsers(isLoadMore = false) {
            if (!isLoadMore) {
                this.userPage = 1;
                this.allUsers = [];
                this.userHasMore = true;
            }
            if (!this.userHasMore || this.userLoading) {
                return;
            }

            this.userLoading = true;
            queryUser({ username: this.leftSearchValue }, { page: this.userPage, limit: this.userLimit }).then(res => {
                const newUsers = res.data.rows.map((item) => ({
                    key: item.username,
                    title: item.username,
                }));
                this.allUsers = [...this.allUsers, ...newUsers];
                this.userTotal = res.data.total;
                this.userHasMore = this.allUsers.length < this.userTotal;
                this.userPage++;

                // 确保所有已选用户都在列表中
                this.selectedUserKeys.forEach(key => {
                    if (!this.allUsers.some(user => user.key === key)) {
                        this.allUsers.push({ key, title: key });
                    }
                });
            }).catch(error => {
                console.error('Error fetching users:', error);
            }).finally(() => {
                this.userLoading = false;
            });
        },
        getGroups(isLoadMore = false) {
            if (!isLoadMore) {
                this.groupPage = 1;
                this.allGroups = [];
                this.groupHasMore = true;
            }
            if (!this.groupHasMore || this.groupLoading) {
                return;
            }

            this.groupLoading = true;
            getGroupList({ page: this.groupPage, limit: this.groupLimit, groupName: this.leftGroupSearchValue }).then(res => {
                const newGroups = res.data.rows.map((item) => ({
                    key: item.id,  // 改为使用 id
                    id: item.id,   // 添加 id 字段
                    title: item.groupName,
                }));
                this.allGroups = [...this.allGroups, ...newGroups];
                this.groupTotal = res.data.total;
                this.groupHasMore = this.allGroups.length < this.groupTotal;
                this.groupPage++;

                // 确保所有已选用户组都在列表中
                this.selectedGroupKeys.forEach(key => {
                    if (!this.allGroups.some(group => group.key === key)) {
                        this.allGroups.push({ key, title: key });
                    }
                });
            }).catch(error => {
                console.error('Error fetching groups:', error);
            }).finally(() => {
                this.groupLoading = false;
            });
        },
        // onRepositoriesChange() {
        //     const checkedValues = this.repositoriesOptions.filter(item => item.enabled).map(item => item.value)
        //     this.repositoriesCheckedList = checkedValues;
        //     this.repositoriesCheckAll = checkedValues.length === 4
        //     this.userAuthMap[this.currentUserIndex] = checkedValues
        // },
        // onRepositoriesCheckAllChange() {
        //     this.repositoriesCheckAll = !this.repositoriesCheckAll;
        //     this.repositoriesCheckedList = this.repositoriesCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
        //     this.userAuthMap[this.currentUserIndex] = this.repositoriesCheckedList
        //     this.repositoriesOptions.forEach(item => {
        //         item.enabled = this.repositoriesCheckAll
        //     })
        // },

        onGroupChange() {
            const checkedValues = this.repositoriesOptions.filter(item => item.enabled).map(item => item.value)
            this.groupCheckedList = checkedValues;
            this.groupCheckAll = checkedValues.length === 4
            this.groupAuthMap[this.currentGroupIndex] = checkedValues
        },
        onGroupCheckAllChange(e) {
            const checkedValue = e.target.checked;
            this.selectedGroupPermissions = checkedValue ? this.permissionOptions.map(option => option.value) : [];
            this.groupIndeterminate = false;
            this.groupCheckAll = checkedValue;
            this.handleGroupPermissionChange(this.selectedGroupPermissions);
        },
        openSelectModal(type) {
            const selectedRows = type === 'USER' ? this.userSelectList : this.groupSelectList
            this.$refs.selectUserGroup.openModal(type, selectedRows, this.isStorageAdmin);
        },
        async getStorageList(page = 1) {
            await new Promise((resolve, reject) => {
                queryStorages({
                    page,
                    limit: 10
                }).then(res => {
                    this.storageList = res.data.rows;
                    this.storageTotal = res.data.total
                    resolve()
                }).catch(e => {
                    reject(e)
                })
            })
        },
        async getRepositoriesList(page = 1) {
            await new Promise((resolve, reject) => {
                queryRepositories({
                    page,
                    limit: 20
                }).then(res => {
                    this.spinning = false
                    this.repositoriesList = []
                    res.data.rows.forEach(item => {
                        item.key = `${item.storageId}/${item.id}`
                        this.repositoriesList.push({ ...item })
                    })
                    this.repositoriesTotal = res.data.total
                    resolve()
                }).catch(e => {
                    reject(e)
                    this.spinning = false
                })
            })
        },
        handleRadioChange(e) {
            this.selectedRowKeys = [];
            this.getList();
        },
        onSelectChange(selectedRowKeys) {
            this.selectedRowKeys = selectedRowKeys;
        },
        async getList(page = 1) {
            this.spinning = true;
            const queryMethod = this.radioModel === 'StorageSpace' ? queryStorages : queryRepositories;
            try {
                const res = await queryMethod({ page, limit: 10 });
                if (this.radioModel === 'StorageSpace') {
                    this.storageList = res.data.rows;
                    this.storageTotal = res.data.total;
                } else {
                    this.repositoriesList = res.data.rows.map(item => ({
                        ...item,
                        key: `${item.storageId}/${item.id}`,
                    }));
                    this.repositoriesTotal = res.data.total;
                }
            } catch (error) {
                console.error('Error fetching data:', error);
            } finally {
                this.spinning = false;
            }
        },
        handleTableChange(pagination) {
            this.getList(pagination.current);
            this.storagePage=pagination.current;
        },
        selectUserGroupChange(val, type) {
            if (type === 'USER') {
                this.userSelectList = val
                this.userSelectCopyList = val
                this.currentUserIndex = val[0]?.key || 0
                const userKeys = val.map(item => `${item.key}`)
                for (const key in this.userAuthMap) {
                    if (!userKeys.includes(key)) this.userAuthMap[key] = []
                }
                this.repositoriesCheckedList = this.userAuthMap[this.currentUserIndex] || []
                this.repositoriesOptions.forEach(item => {
                    item.enabled = this.repositoriesCheckedList.includes(item.value)
                })
            } else {
                this.groupSelectList = val
                this.groupSelectCopyList = val
                this.currentGroupIndex = val[0]?.key || 0
                const groupKeys = val.map(item => `${item.key}`)
                for (const key in this.groupAuthMap) {
                    if (!groupKeys.includes(key)) this.groupAuthMap[key] = []
                }
                this.groupCheckedList = this.groupAuthMap[this.currentGroupIndex] || []
                this.repositoriesOptions.forEach(item => {
                    item.enabled = this.groupCheckedList.includes(item.value)
                })
            }
        },
        userClick(item) {
            this.currentUserIndex = item.key
            if (this.userAuthMap[item.key]) {
                this.repositoriesCheckedList = this.userAuthMap[item.key]
            } else {
                this.userAuthMap[item.key] = []
                this.repositoriesCheckedList = []
            }
            this.repositoriesCheckAll = this.repositoriesCheckedList.length === 4
            this.repositoriesOptions.forEach(item => {
                item.enabled = this.repositoriesCheckedList.includes(item.value)
            })
        },
        groupClick(item) {
            this.currentGroupIndex = item.key
            if (this.groupAuthMap[item.key]) {
                this.groupCheckedList = this.groupAuthMap[item.key]
            } else {
                this.groupAuthMap[item.key] = []
                this.groupCheckedList = []
            }
            this.groupCheckAll = this.groupCheckedList.length === 4
            this.repositoriesOptions.forEach(item => {
                item.enabled = this.groupCheckedList.includes(item.value)
            })
        },
        handleConfirm() {
            this.$refs.form.validate(validate => {
                if (validate) {
                    if (!this.selectedResources.length && !this.isAdmin) {
                        this.$message.error(this.$t('Permissions.AtLeastOneRepository'))
                        return
                    }
                    const groups = Object.entries(this.groupPermissions).map(([key, value]) => ({
                        id: key,
                        access: value || []
                    }));
                    const users = Object.entries(this.userPermissions).map(([key, value]) => ({
                        id: key,
                        access: value || []
                    }));
                    if (this.selectedResources.length) {
                        if (users.some(item => !item.access.length) && !this.isStorageAdmin && !this.isStorageUser) {
                            this.$message.error(this.$t('Permissions.UserNoneSelectAuth'))
                            return
                        }
                        if (groups.some(item => !item.access.length) && !this.isStorageAdmin && !this.isStorageUser) {
                            this.$message.error(this.$t('Permissions.GroupNoneSelectAuth'))
                            return
                        }
                    }
                    this.confirmLoading = true
                    const params = {
                        description: this.form.description,
                        name: this.form.name,
                        privileges: {
                            groups,
                            users,
                        },
                        resources: this.selectedResources
                    }
                    if (this.isAnonymous) {
                        params.access = this.anonymousPermissions;

                    }

                    const method = this.isEdit ? updatePermission : createPermission;
                    method(params).then(res => {
                        this.visible = false;
                        this.$emit('reset');
                    }).finally(() => {
                        this.confirmLoading = false
                    })
                }
            })
        },
        handleVisibleChange(visible) {
            if (!visible || this.isView) {
                this.confirmVisible = false;
                return;
            }
            if (this.isStorageAdmin && this.userSelectList.length) {
                this.confirmVisible = true
            } else {
                this.confirmVisible = false;
                this.openSelectModal('USER')
            }
        },
        onStorageSelectChange(selectedStorageKeys) {
            const previouslySelected = this.selectedStorageKeys;
            this.selectedStorageKeys = selectedStorageKeys;

            // 找出新选中和取消选中的存储空间
            const newlySelected = selectedStorageKeys.filter(key => !previouslySelected.includes(key));

            // 处理新中的存储空间 取消仓库的选择
            newlySelected.forEach(storageId => {
                this.selectedRepositoryKeys = this.selectedRepositoryKeys.filter(item => item.startsWith('storageId' + '/'))
            });

        },
        onRepositoriesChange(selectedRepositoryKeys, selectedRows) {
            this.selectedRepositoryKeys = selectedRepositoryKeys;
            if (selectedRows.length === 0) return;
            const storageId = selectedRows[0].storageId;
            this.selectedStorageKeys = this.selectedStorageKeys.filter(item => item !== storageId);
        },

        checkStorageSelection(storageId) {
            const storage = this.storageList.find(s => s.id === storageId);
            if (!storage || !storage.repositories) return;
            const allRepositoryIds = storage.repositories.map(repo => repo.id);
            const selectedRepositoryIds = this.selectedRepositoryKeys.filter(key =>
                storage.repositories.some(repo => repo.id === key)
            );
            // 如果该存储空间的所有仓库都被选中，则选中该存储空间
            if (allRepositoryIds.length === selectedRepositoryIds.length) {
                if (!this.selectedStorageKeys.includes(storageId)) {
                    this.selectedStorageKeys = [...this.selectedStorageKeys, storageId];
                }
            } else {
                // 如果不是所有仓库都被选中，则取消选中该存储空间
                this.selectedStorageKeys = this.selectedStorageKeys.filter(key => key !== storageId);
            }
        },


        onExpand(expanded, record) {
            const key = record.id;
            const index = this.expandedRowKeys.indexOf(key);
            if (expanded && index === -1) {
                this.expandedRowKeys.push(key);
                if (!record.repositories || record.repositories.length === 0) {
                    this.loadRepositories(record.id);
                }
            } else if (!expanded && index > -1) {
                this.expandedRowKeys.splice(index, 1);
            }
        },

        async loadRepositories(storageId) {
            try {
                const params = {
                    storageId: storageId,
                    limit: 1000,
                    page: 1
                };
                const res = await queryRepositoriesByStorage(params);
                const storageIndex = this.storageList.findIndex(storage => storage.id === storageId);
                if (storageIndex > -1) {
                    const repositories = res.data.rows.map(item => {
                        const path = this.selectedPath.find(p => p.storageId === item.storageId && p.repositoryId === item.id);
                        if (path) {
                            return {
                                id: storageId + '/' + item.id,
                                title: item.id,
                                currentInPattern: '',
                                isInError: false,
                                storageId: item.storageId,
                                includes: [path.path]
                            }
                        } else {
                            return {
                                id: storageId + '/' + item.id,
                                title: item.id,
                                includes: [],
                                currentInPattern: '',
                                isInError: false,
                                storageId: item.storageId,
                            }
                        }
                    });

                    this.$set(this.storageList[storageIndex], 'repositories', repositories);
                }
            } catch (error) {
                console.error('Error fetching repositories:', error);
                // 如果加载失败，设置为空数组
                const storageIndex = this.storageList.findIndex(storage => storage.id === storageId);
                if (storageIndex > -1) {
                    this.$set(this.storageList[storageIndex], 'repositories', []);
                }
            }
        },
        handleAddRowInPattern(record) {
            if (record.currentInPattern.trim()) {
                const repository = this.selectedRepositoryKeys.find(s => s === record.id);
                if (!repository) {
                    this.selectedRepositoryKeys.push(record.id);
                }
                if (record.includes.indexOf(record.currentInPattern) === -1) {
                    record.includes.unshift(record.currentInPattern)
                    const item = {
                        storageId: record.storageId,
                        repositoryId: record.title,
                        path: record.currentInPattern,
                    }
                    this.selectedPath.push(item);
                    record.currentInPattern = ''
                } else {
                    record.isInError = true
                }
            }
        },
        removePath(record, i) {
            const index = this.selectedPath.findIndex(item => (item.storageId === record.storageId && item.repositoryId === record.title && item.path === record.includes[i]));
            this.selectedPath.splice(index, 1);
            record.includes.splice(i, 1);
        },

    

        handleUserTransferChange(nextTargetKeys, direction, moveKeys) {
            if (direction === 'right') {
                // 处理向右移动（添加）用户的情况
                moveKeys.forEach(key => {
                    if (!this.userPermissions[key]) {
                        // 如果用户没有权限，初始化为空数组
                        this.$set(this.userPermissions, key, []);
                    }
                });
            } else {
                moveKeys.forEach(key => {
                    // 从权限对象中删除该用户
                    this.$delete(this.userPermissions, key);
                });
            }

            this.selectedUserKeys = nextTargetKeys;

            // 检查是否需要加载更多用户
            const leftListCount = this.allUsers.length - this.selectedUserKeys.length;
            const threshold = Math.min(this.userLimit, 10);

            if (leftListCount <= threshold && this.userHasMore && !this.userLoading) {
                this.$nextTick(() => {
                    this.getUsers(true);
                });
            }

        },

        handleUserSelectChange(sourceSelectedKeys, targetSelectedKeys) {
            this.selectedUserItems = targetSelectedKeys;
            if (targetSelectedKeys.length === 1) {
                // 单选情况
                const selectedUser = targetSelectedKeys[0];
                this.selectedUserPermissions = this.userPermissions[selectedUser] || [];
            } else if (targetSelectedKeys.length > 1) {
                // 多选情况
                const permissions = targetSelectedKeys.map(key => this.userPermissions[key] || []);
                this.selectedUserPermissions = permissions.reduce((acc, curr) =>
                    acc.filter(perm => curr.includes(perm)),
                    this.permissionOptions.map(option => option.value)
                );
            } else {
                // 没有选择用户
                this.selectedUserPermissions = [];

            }
            if (this.selectedUserPermissions.length === 0) {
                this.checkAll = false;
                this.indeterminate = false;
            } else if (this.selectedUserPermissions.length === 3) {
                this.checkAll = true;
                this.indeterminate = false;
            } else {
                this.indeterminate = true;
            }

            this.currentSelectedUser = targetSelectedKeys.length === 1 ? targetSelectedKeys[0] : null;
        },
        handleUserPermissionChange(checkedValues) {
            // 更新所有选中用户的权限
            const checkedCount = checkedValues.length;
            this.checkAll = checkedCount === this.permissionOptions.length;
            this.indeterminate = checkedCount > 0 && checkedCount < this.permissionOptions.length;
            if (this.currentSelectedUser) {
                this.$set(this.userPermissions, this.currentSelectedUser, checkedValues);
            } else {
                // 如果是多选，更新所有选中用户的权限
                this.selectedUserItems.forEach(userKey => {
                    this.$set(this.userPermissions, userKey, [...checkedValues]);
                });
            }
        },

        onCheckAllChange(e) {
            const checkedValue = e.target.checked;
            this.selectedUserPermissions = checkedValue ? this.permissionOptions.map(option => option.value) : [];
            this.indeterminate = false;
            this.checkAll = checkedValue;
            this.handleUserPermissionChange(this.selectedUserPermissions);
        },

        updateSelectedPermissions() {
            // 获取所有选中用户的权限交集
            if (this.selectedUserKeys.length === 1) {
                const selectedUser = this.selectedUserKeys[0];
                this.selectedUserPermissions = this.userPermissions[selectedUser] || [];
            } else {
                // 如果选择了多个用户或没有选择用户，清空权限选择
                this.selectedUserPermissions = [];
            }
        },

        getUserRowSelection({ selectedKeys, itemSelectAll, itemSelect }) {
            return {
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
                columnWidth: 30
            };
        },



        getUsers(isLoadMore = false) {
            if (!isLoadMore) {
                this.userPage = 1;
                this.allUsers = [];
                this.userHasMore = true;
            }
            if (!this.userHasMore || this.userLoading) {
                return;
            }

            this.userLoading = true;
            queryUser({ username: this.leftSearchValue }, { page: this.userPage, limit: this.userLimit }).then(res => {
                const newUsers = res.data.rows.map((item) => ({
                    key: item.username,
                    title: item.username,
                }));
                this.allUsers = [...this.allUsers, ...newUsers];
                this.userTotal = res.data.total;
                this.userHasMore = this.allUsers.length < this.userTotal;
                this.userPage++;

                // 确保所有已选用户都在列表中
                this.selectedUserKeys.forEach(key => {
                    if (!this.allUsers.some(user => user.key === key)) {
                        this.allUsers.push({ key, title: key });
                    }
                });
                if(this.allUsers.length===this.selectedUserKeys.length&&this.userHasMore){
                    this.userLoading = false;
                    this.getUsers(true);
                }
            }).catch(error => {
                console.error('Error fetching users:', error);
            }).finally(() => {
                this.userLoading = false;
            });
        },
        handleScroll(direction, e) {
            if (direction === 'left') {
                this.$nextTick(() => {
                    const target = e.target;
                    const { scrollTop, clientHeight, scrollHeight } = target;
                    if (scrollTop + clientHeight >= scrollHeight - 50) {
                        if (!this.userLoading && this.userHasMore) {
                            this.getUsers(true);
                        }
                    }
                });
            }


        },
        handleFilter(inputValue, item) {
            return item.title.toLowerCase().indexOf(inputValue.toLowerCase()) !== -1;
        },
        onSelectChange(selectedRowKeys) {
            this.selectedRowKeys = selectedRowKeys;
        },
        resetData() {
            this.page = 1;
            this.allUsers = [];
            this.hasMore = true;
            this.selectedUserKeys = [];
            this.leftSearchValue = '';
        },
        getGroups(isLoadMore = false) {
            if (!isLoadMore) {
                this.groupPage = 1;
                this.allGroups = [];
                this.groupHasMore = true;
            }
            if (!this.groupHasMore || this.groupLoading) {
                return;
            }

            this.groupLoading = true;
            getGroupList({ page: this.groupPage, limit: this.groupLimit, groupName: this.leftGroupSearchValue }).then(res => {
                const newGroups = res.data.rows.map((item) => ({
                    key: item.id,  // 改为使用 id
                    id: item.id,   // 添加 id 字段
                    title: item.groupName,
                }));
                this.allGroups = [...this.allGroups, ...newGroups];
                this.groupTotal = res.data.total;
                this.groupHasMore = this.allGroups.length < this.groupTotal;
                this.groupPage++;

                // 确保所有已选用户组都在列表中
                this.selectedGroupKeys.forEach(key => {
                    if (!this.allGroups.some(group => group.key === key)) {
                        this.allGroups.push({ key, title: key });
                    }
                });
                if(this.allGroups.length===this.selectedGroupKeys.length&&this.groupHasMore){
                    this.groupLoading = false;
                    this.getGroups(true);
                }
            }).catch(error => {
                console.error('Error fetching groups:', error);
            }).finally(() => {
                this.groupLoading = false;
            });
        },


        handleGroupTransferChange(nextTargetKeys, direction, moveKeys) {
            if (direction === 'right') {
                moveKeys.forEach(key => {
                    if (!this.groupPermissions[key]) {
                        this.$set(this.groupPermissions, key, []);
                    }
                });
            } else {
                moveKeys.forEach(key => {
                    this.$delete(this.groupPermissions, key);
                });
            }

            this.selectedGroupKeys = nextTargetKeys;

            const leftListCount = this.allGroups.length - this.selectedGroupKeys.length;
            const threshold = Math.min(this.groupLimit, 10);

            if (leftListCount <= threshold && this.groupHasMore && !this.groupLoading) {
                this.$nextTick(() => {
                    this.getGroups(true);
                });
            }
        },

        updateGroupCheckStatus() {
            if (this.selectedGroupKeys.length === 0) {
                this.selectedGroupPermissions = [];
                this.groupCheckAll = false;
                this.groupIndeterminate = false;
            } else if (this.selectedGroupKeys.length === 1) {
                const selectedGroup = this.selectedGroupKeys[0];
                this.selectedGroupPermissions = this.groupPermissions[selectedGroup] || [];
            } else {
                const permissions = this.selectedGroupKeys.map(key => this.groupPermissions[key] || []);
                this.selectedGroupPermissions = permissions.reduce((acc, curr) =>
                    acc.filter(perm => curr.includes(perm)),
                    this.permissionOptions.map(option => option.value)
                );
            }

            const checkedCount = this.selectedGroupPermissions.length;
            this.groupCheckAll = checkedCount === this.permissionOptions.length;
            this.groupIndeterminate = checkedCount > 0 && checkedCount < this.permissionOptions.length;
        },


        handleGroupSelectChange(sourceSelectedKeys, targetSelectedKeys) {
            this.selectedGroupItems = targetSelectedKeys;
            if (targetSelectedKeys.length === 1) {
                const selectedGroup = targetSelectedKeys[0];
                this.selectedGroupPermissions = this.groupPermissions[selectedGroup] || [];
            } else if (targetSelectedKeys.length > 1) {
                const permissions = targetSelectedKeys.map(key => this.groupPermissions[key] || []);
                this.selectedGroupPermissions = permissions.reduce((acc, curr) =>
                    acc.filter(perm => curr.includes(perm)),
                    this.permissionOptions.map(option => option.value)
                );
            } else {
                this.selectedGroupPermissions = [];
            }
            this.currentSelectedGroup = targetSelectedKeys.length === 1 ? targetSelectedKeys[0] : null;
            this.updateGroupCheckStatus;
        },

        getGroupRowSelection({ selectedKeys, itemSelectAll, itemSelect }) {
            return {
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
                columnWidth: 30
            };
        },

        handleGroupPermissionChange(checkedValues) {
            const checkedCount = checkedValues.length;
            this.groupCheckAll = checkedCount === this.permissionOptions.length;
            this.groupIndeterminate = checkedCount > 0 && checkedCount < this.permissionOptions.length;

            if (this.currentSelectedGroup) {
                this.$set(this.groupPermissions, this.currentSelectedGroup, checkedValues);
            } else {
                // 如果是多选，更新所有选中用户组的权限
                this.selectedGroupItems.forEach(groupKey => {
                    this.$set(this.groupPermissions, groupKey, [...checkedValues]);
                });
            }
        },

        

        updateSelectedGroupPermissions() {
            if (this.selectedGroupKeys.length === 1) {
                const selectedGroup = this.selectedGroupKeys[0];
                this.selectedGroupPermissions = this.groupPermissions[selectedGroup] || [];
            } else {
                this.selectedGroupPermissions = [];
            }
        },

        handleGroupScroll(direction) {
            if (direction === 'left') {
                this.$nextTick(() => {
                    const leftList = this.$refs.leftGroupList;
                    if (leftList) {
                        const listBody = leftList.$el.querySelector('.ant-transfer-list-content');
                        if (listBody) {
                            const { scrollTop, clientHeight, scrollHeight } = listBody;
                            if (scrollTop + clientHeight >= scrollHeight - 50) {
                                if (!this.groupLoading && this.groupHasMore) {
                                    this.getGroups(true);
                                }
                            }
                        }
                    }
                });
            }
        },

        handleGroupFilter(inputValue, item) {
            return item.title.toLowerCase().indexOf(inputValue.toLowerCase()) !== -1;
        },

        handleLeftGroupSearch(e) {
            this.leftGroupSearchValue = e.target.value;
            this.getGroups();
        },

        resetGroupData() {
            this.groupPage = 1;
            this.allGroups = [];
            this.groupHasMore = true;
            this.selectedGroupKeys = [];
            this.leftGroupSearchValue = '';
        },

        onSearch(record) {
            // 触发搜索
            this.$set(this.loadRepositories, record.id, record.searchQuery);
            // 保持输入框展开
            this.$set(this.searchExpandedMap, record.id, true);
        },

        filteredRepositories(record) {
            const searchQuery = this.loadRepositories[record.id] || '';
            return record.repositories.filter(repo =>
                repo.title.toLowerCase().includes(searchQuery.toLowerCase())
            );
        },
        toggleSearch(record) {
            const currentValue = this.searchExpandedMap[record.id] || false;
            this.$set(this.searchExpandedMap, record.id, !currentValue);
        },
        showResourceModal() {
            this.resourceModalVisible = true;
            this.storagePage=1;
            this.getStorageList();
            
        },
        handleResourceModalOk() {
            this.resourceModalVisible = false;
            this.selectedResources = [];
            this.selectedStorageKeys.forEach(key => {
                const item = {
                    title: key,
                    storageId: key,
                    type: 'storage',
                }
                this.selectedResources.push(item);
            })
            this.selectedRepositoryKeys.forEach(key => {
                const [storageId, repositoryId] = key.split('/');
                const paths = this.selectedPath.filter(e => e.storageId === storageId && e.repositoryId === repositoryId);
                if (paths.length == 0) {
                    const item = {
                        title: key,
                        storageId,
                        repositoryId,
                        type: 'repository',
                    }
                    this.selectedResources.push(item);
                }
                paths.forEach(e => {
                    const item = {
                        title: key,
                        storageId,
                        repositoryId,
                        path: e.path,
                        type: 'path',
                    }
                    this.selectedResources.push(item);
                })
            })
            this.expandedRowKeys=[];
        },
        handleResourceModalCancel() {
            this.resourceModalVisible = false;
            this.expandedRowKeys=[];
        },
        removeUser(item) {
            const index = this.selectedUserKeys.indexOf(item.key);
            if (index > -1) {
                this.selectedUserKeys.splice(index, 1);
            }
        },
        removeGroup(item) {
            const index = this.selectedGroupKeys.indexOf(item.key);
            if (index > -1) {
                this.selectedGroupKeys.splice(index, 1);
            }
        },
    },
}
</script>

<style scoped lang="scss">
.resource-item {
    border: 1px solid #c9d0e3;
    border-radius: 4px;
    display: flex;
    align-items: center;
    justify-content: center;
    flex-direction: column;
    padding: 32px;
    cursor: pointer;
    font-size: 14px;
    font-weight: 600;

    span {
        margin-top: 16px;
    }

    &:hover {
        span {
            color: #1890FF;
        }
    }
}

.select-content {
    padding-right: 20px;
    border-right: 1px solid #e9e9e9;

    .selected-list {
        height: calc(100vh - 360px);
        overflow-y: auto;

        .selected-item {
            padding: 8px;
            margin-top: 6px;
            border-radius: 6px;
            cursor: pointer;

            &:hover {
                background: #f1f3f8;
            }
        }

        .active {
            background: rgba(24, 144, 255, 0.1);

            &:hover {
                background: rgba(24, 144, 255, 0.1);
            }
        }
    }
}


.title {
    font-size: 14px;
    font-weight: 600;
    line-height: 1.5;
    margin-bottom: 12px;
}

.permission-item {
    border-bottom: 1px solid #e9e9e9;
    margin-bottom: 24px;
    padding-bottom: 20px;

    &:last-child {
        border-bottom: none;
    }
}

.title .custom-badge {
    top: -2px;
    right: -36px;
}

.tab-container {
    margin-bottom: 20px;
    width: 100%;
}

.custom-tabs {
    /deep/ .ant-tabs-nav {
        margin-bottom: 0;
        width: 100%;
    }

    /deep/ .ant-tabs-nav-list {
        width: 100%;
        display: flex;
    }

    /deep/ .ant-tabs-tab {
        flex: 1;
        margin: 0;
        padding: 0;
        align-items: center;
        text-align: center;
        justify-content: center;
        background-color: #f5f5f5;
        border: none;
        border-radius: 0;
        transition: all 0.3s;
        position: relative;
        overflow: visible;
        height: 30px;
        line-height: 30px;
        width: 33.33%;
        color: #333; // 默认字体颜色

        &:not(:last-child) {
            border-right: 1px solid #d9d9d9;
        }

        &::after {
            content: '';
            position: absolute;
            right: -15px;
            top: 0;
            width: 0;
            height: 0;
            border-top: 15px solid transparent;
            border-bottom: 15px solid transparent;
            border-left: 15px solid #f5f5f5;
            z-index: 1;
            transition: border-left-color 0.3s;
        }

        &:last-child::after {
            display: none;
        }
    }

    /deep/ .ant-tabs-tab-btn {
        width: 100%;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        font-size: 14px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        padding: 0 10px;
        color: inherit;
    }

    /deep/ .ant-tabs-tab-active {
        background-color: #1890ff;
        color: white !important;

        &::after {
            border-left-color: #1890ff; // 选中状态下改变箭头颜色
        }
    }

    /deep/ .ant-tabs-ink-bar {
        display: none;
    }
}

/deep/ .ant-table-expanded-row {
    .ant-table-row {
        background-color: #fafafa;
    }
}

.single-pattern-item {
    background: #f8fafb;
    color: #999db4;
    border: solid 1px #c9d0e3;
    padding-right: 40px;
    padding-left: 12px;
    line-height: 40px;
    border-radius: 6px;
    position: relative;

    .close-icon {
        position: absolute;
        right: 12px;
        top: 12px;
    }
}

.insert-item {
    position: relative;

    ::v-deep .ant-form-item {
        margin-bottom: 0;
    }

    .plus-icon {
        position: absolute;
        right: 12px;
        top: 2px;
    }

    .per-plus-icon {
        position: absolute;
        right: 12px;
        top: 12px;
    }
}

.transfer-container {
    /deep/ .ant-transfer-list .ant-transfer-list-content {
        height: calc(100% - 55px) !important;
    }

}

/deep/ .ant-form-item-label {
    flex: 0 0 auto;
    width: auto;
    max-width: 33%; // 限制label的最大宽度
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;

    >label {
        font-weight: normal;
    }
}

/deep/ .ant-form-item-control-wrapper {
    flex: 1;
    max-width: calc(100% - 8px); // 为label预留一些空间
}

/deep/ .ant-input {
    width: 100%;
}

/deep/ .ant-form-item {
    margin-bottom: 0;
    display: flex;
    align-items: center;
}

.selected-summary {
    margin-bottom: 5px;
    padding: 15px;
    background-color: #f0f2f5;
    border-radius: 4px;

    h4 {
        margin-bottom: 10px;
        font-weight: 400;
    }

    .summary-item {
        margin-bottom: 5px;

        strong {
            margin-right: 5px;
        }
    }
}

.expanded-row-content {
    padding: 0;
    display: flex;
    flex-direction: column;
}

.repository-search {
    align-self: flex-end;
    margin-bottom: 16px;
}

/deep/ .repository-table {
    .ant-table-thead>tr>th {
        padding: 8px 4px; // 减小表头的内边距
        height: 40px; // 设置固定的表头高度
        line-height: 24px; // 调整行高以垂直居中内容
    }

    .ant-table-tbody>tr>td {
        padding: 4px; // 减小单元格内边距
        height: 32px; // 减小单元格高度
        vertical-align: middle; // 保持垂直居中对齐
    }

    .ant-table-thead>tr>th {
        background-color: #fafafa; // 保持表头背景色不变
    }
}

// 特别针对展开行的样式
/deep/ .ant-table-expanded-row {

    >td {
        padding: 0;
    }

    .repository-table {
        .ant-table-tbody>tr>td {
            padding: 2px 4px; // 进一步减小展开行中表格的单元格内边距
            height: 28px; // 减小展开行中表格的单元格高度
        }
    }
}

.search-slot {
    display: flex;
    align-items: center;
    justify-content: space-between; // 在标题和搜索框之间添加空间
    height: 30px; // 减小高度以使表头更紧凑
}

.search-container {
    display: flex;
    align-items: center;

    .ant-input {
        width: 0;
        padding: 2px;
        border: none;
        transition: all 0.3s;
        height: 30px; // 减小输入框高度

        &.expanded {
            width: 200px;
            margin-left: 8px;
            padding: 2px 8px; // 减小内边距
            border: 1px solid #d9d9d9;
        }
    }

    .search-icon {
        margin-left: 8px;
        cursor: pointer;
        transition: all 0.3s;
        font-size: 14px; // 减小图标大小

        &:hover {
            color: #1890ff;
        }
    }
}

/deep/ .ant-table-wrapper {
    .ant-table {
        background-color: transparent;

        .ant-table-thead>tr>th {
            background-color: transparent;
            border-bottom: 1px solid #f0f0f0;
        }

        .ant-table-tbody>tr>td {
            border-bottom: none;
        }

        .ant-table-tbody>tr:hover>td {
            background-color: transparent;
        }
    }
}

.ant-list {
    border: 1px solid #e8e8e8;
    border-radius: 4px;
}

.ant-list-item {
    padding: 8px 16px;
    transition: background-color 0.3s;

    &:hover {
        background-color: #f5f5f5;
    }
}

.permissions-checkbox-container {
    border: 1px solid #d9d9d9;
    border-radius: 6px;
    background-color: #fff;
    height: 400px;
    overflow-y: auto;
    padding: 0; // 移除顶部内边距
    margin-left: 10px;

    .permission-title {
        font-size: 14px;
        padding: 8px 16px; // 添加左右内边距
        border-bottom: 1px solid #d9d9d9; // 使用与外边框相同的颜色
        margin-bottom: 8px;
    }

    .ant-checkbox-group {
        display: flex;
        flex-direction: column;
        padding: 0 16px; // 为复选框组添加左右内边距
    }

    .ant-checkbox-wrapper {
        margin-bottom: 16px; // 增加复选框之间的间距
        margin-left: 0;
        display: flex;
        align-items: center; // 让复选框和文字顶部对齐

        &:last-child {
            margin-bottom: 0; // 最后一个复选框不需要底部间距
        }
    }
}

.custom-right-table {
    /deep/ .ant-table-tbody>tr>td {
        border-bottom: none !important;
        padding: 0 !important;
        height: 32px;
        line-height: 32px;

        .ant-table-selection-column {
            width: 30px !important;
        }

    }

    /deep/ .ant-table-tbody>tr>td>span {
        text-align: left;
        padding: 0;
        width: 30px;
    }

    /deep/ .ant-table-tbody>tr:hover>td {
        background-color: transparent;
    }

    /deep/ .ant-table-row {
        background-color: transparent !important;
    }

    /deep/ .ant-table-row:hover {
        background-color: #e6f7ff !important;
    }

    /deep/ .ant-table-row-selected>td {
        background-color: #e6f7ff !important;
    }

    /deep/ .ant-checkbox-wrapper {
        margin-left: 8px;
    }

    /deep/ .ant-table-row>td>span {
        padding: 0 8px;
        display: block;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
    }
}

/deep/ .ant-table-row-expand-icon-cell {
    width: 30px !important;
    padding: 0 !important;
}

/deep/ .ant-table-expand-icon-th {
    width: 30px !important;
    padding: 0 !important;
}

.resource-modal {
    /deep/ .ant-modal-body {
        padding: 0;
    }
}

.selected-resources-table {
    /deep/ .ant-table-content {
        height: calc(100% - 300px);
        overflow-y: auto;
    }
}

.ant-drawer-wrapper-body {
    display: flex;
    flex-direction: column;
    height: 100%;
}

.ant-drawer-body {
    flex: 1;
    overflow-y: auto;
    padding-bottom: 55px;
    /* 为 footer 留出空间 */
}

.drawer-footer {
    position: absolute;
    bottom: 0;
    width: 100%;
    border-top: 1px solid #e8e8e8;
    padding: 10px 16px;
    text-align: right;
    left: 0;
    background: #fff;
    border-radius: 0 0 4px 4px;
}

.selected-resources-table {
    /deep/ .ant-table-body {
        max-height: calc(100vh - 450px);
        /* 调整这个值以适应你的布局 */
        overflow-y: auto;
    }
}

.anonymous-permissions-group {
    display: flex;
    flex-direction: row !important;
    flex-wrap: wrap;

    /deep/ .ant-checkbox-wrapper {
        margin-right: 16px;
    }
    /deep/ .ant-card-body {
        padding: 8px;
    }
}
</style>
