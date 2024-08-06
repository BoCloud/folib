<template>
    <a-drawer
        placement="right"
        width="65%"
        :title="(isEdit ? $t('Permissions.Edit') : $t('Permissions.Create'))"
        :visible="visible"
        @close="closeModal"
    >
        <a-spin :spinning="spinning">
            <a-form-model ref="form" :model="form" >
                <a-form-model-item
                    prop="name"
                    :rules="[
                        { required: true, message: $t('Permissions.EnterTheNameCreate'), trigger: 'blur' },
                        { pattern: /^[0-9A-Za-z_-]+$/, message: $t('Permissions.EnterTheNamePattern') }
                    ]"
                >
                    <a-input
                        v-model="form.name"
                        :placeholder="$t('Permissions.EnterTheNameCreate')"
                        :maxLength="100"
                    />
                </a-form-model-item>
            </a-form-model>
            <a-steps v-model="step" size="small" class="step">
                <a-step :title="$t('Permissions.Resources')" :status="step === 0 ? 'process' : 'wait'" :description="$t('Permissions.ResourcesDesc')"/>
                <a-step :title="$t('Permissions.Users')" :status="step === 1 ? 'process' : 'wait'" :description="$t('Permissions.UsersDesc')"/>
                <a-step :title="$t('Permissions.Groups')" :status="step === 2 ? 'process' : 'wait'" :description="$t('Permissions.GroupsDesc')"/>
            </a-steps>
            <div v-show="!step">
                <repositories
                    ref="repositories"
                    :repositoriesList="repositoriesList"
                    :storageList="storageList"
                />
            </div>
            <div v-if="step === 1" class="by-flex by-col-stretch">
                <div class="select-content">
                    <div class="title">{{ $t(`Permissions.SelectedUser`) }}</div>
                    <div class="by-flex by-m-t-10 by-m-b-10">
                        <a-input v-model="userSearch" :placeholder="$t('Permissions.Search')" allow-clear class="by-w-300"></a-input>
                        <a-button type="primary" icon="edit" class="by-m-l-10" @click="openSelectModal('USER')"/>
                    </div>
                    <div class="selected-list">
                        <div
                            class="selected-item by-flex by-row-between"
                            v-for="(item, index) in userSelectList"
                            :key="index"
                            :class="{'active': currentUserIndex === item.key}"
                            @click="userClick(item)"
                        >
                            <span class="by-m-l-10">{{ item.title }}</span>
                            <!-- <a-tooltip placement="topLeft" :title="$t('Permissions.NoPermissionsTip')">
                                <a-icon type="exclamation-circle" />
                            </a-tooltip>-->
                        </div>
                    </div>
                </div>
                <div class="by-p-l-32 by-flex-1">
                    <div class="permission-item">
                        <div class="title">{{ $t(`Permissions.SelectedPermissions`) }}</div>
                        <div class="by-flex">
                            <a-checkbox-group v-model="repositoriesCheckedList" :disabled="!userSelectList.length" :options="repositoriesOptions" @change="onRepositoriesChange" />
                            <a-checkbox :checked="repositoriesCheckAll" :disabled="!userSelectList.length" @change="onRepositoriesCheckAllChange" class="by-m-l-10">
                                {{ $t(`Permissions.SelectAll`) }}
                            </a-checkbox>
                        </div>
                    </div>
                </div>
            </div>
            <div v-if="step === 2" class="by-flex by-col-stretch">
                <div class="select-content">
                    <div class="title">{{ $t(`Permissions.SelectedGroups`) }}</div>
                    <div class="by-flex by-m-t-10 by-m-b-10">
                        <a-input v-model="userSearch" :placeholder="$t('Permissions.Search')" allow-clear class="by-w-300"></a-input>
                        <a-button type="primary" icon="edit" class="by-m-l-10" @click="openSelectModal('GROUP')"/>
                    </div>
                    <div class="selected-list">
                        <div
                            class="selected-item by-flex by-row-between"
                            v-for="(item, index) in groupSelectList"
                            :key="index"
                            :class="{'active': currentGroupIndex === item.key}"
                            @click="groupClick(item)"
                        >
                            <span class="by-m-l-10">{{ item.title }}</span>
                            <!-- <a-tooltip placement="topLeft" :title="$t('Permissions.GroupNoPermissionsTip')">
                                <a-icon type="exclamation-circle" />
                            </a-tooltip>-->
                        </div>
                    </div>
                </div>
                <div class="by-p-l-32 by-flex-1">
                    <div class="permission-item">
                        <div class="title">{{ $t(`Permissions.SelectedPermissions`) }}</div>
                        <div class="by-flex">
                            <a-checkbox-group v-model="repositoriesGroupCheckedList" :disabled="!groupSelectList.length" :options="repositoriesOptions" @change="onRepositoriesGroupChange" />
                            <a-checkbox :checked="repositoriesGroupCheckAll" :disabled="!groupSelectList.length" @change="onRepositoriesGroupCheckAllChange" class="by-m-l-10">
                                {{ $t(`Permissions.SelectAll`) }}
                            </a-checkbox>
                        </div>
                    </div>
                </div>
            </div>
        </a-spin>
        <div class="drawer-footer">
            <a-button :style="{ marginRight: '8px' }" @click="closeModal">
                {{ $t(`Permissions.Cancel`) }}
            </a-button>
            <a-button type="primary" :loading="confirmLoading" @click="handleConfirm">
                {{ $t(`Permissions.Confirm`) }}
            </a-button>
        </div>
        <selectUserGroup
            ref="selectUserGroup"
            @confirm="selectUserGroupChange"
        ></selectUserGroup>
    </a-drawer>
</template>

<script>
import repositories from "./repositories.vue";
import selectUserGroup from "./selectUserGroup.vue";
import { getPermissionDetail, createPermission, getPermissionUsers, updatePermission } from "@/api/permissions";
import { getStorages, getStoragesAndRepositories } from "@/api/folib";
import { getGroupList } from "@/api/group";
import { uniq } from "lodash/array";

export default {
    name: "modal",
    components: {
        repositories,
        selectUserGroup
    },
    data()
    {
        return {
            visible: false,
            isEdit: false,
            spinning: false,
            step: 0,
            form: {
                name: '',
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
            repositoriesGroupCheckedList: [],
            repositoriesGroupCheckAll: false,
            confirmLoading: false,
        }
    },
    computed: {
        repositoriesOptions() {
            return [
                {
                    label: this.$t(`Permissions.Download`),
                    value: 'ARTIFACTS_RESOLVE'
                },
                {
                    label: this.$t(`Permissions.DeployCache`),
                    value: 'ARTIFACTS_DEPLOY'
                },
                {
                    label: this.$t(`Permissions.DeleteUpdate`),
                    value: 'ARTIFACTS_DELETE'
                },
                {
                    label: this.$t(`Permissions.PromoDistribution`),
                    value: 'ARTIFACTS_PROMOTION'
                },
            ]
        }
    },
    watch: {
        userSearch(val) {
            if (!val) {
                this.userSelectList = this.userSelectCopyList
                return
            }
            this.userSelectList = this.userSelectList.filter(item => {
                return item.title.indexOf(val) !== -1
            })
        }
    },
    methods: {
        async openModal(id)
        {
            this.visible = true;
            this.spinning = true;
            this.isEdit = !!id;
            this.init()
            await this.getStorageList()
            await this.getRepositoriesList()
            if (id) {
                this.getDetail(id);
            } else {
                this.spinning = false
            }
        },
        closeModal()
        {
            this.visible = false;
        },
        init() {
            this.form.name = ''
            this.step = 0
            this.userSelectList = []
            this.userSelectCopyList = []
            this.userAuthMap = {}
            this.currentUserIndex = 0
            this.repositoriesCheckedList = []
            this.repositoriesCheckAll = false

            this.groupSelectList = []
            this.groupSelectCopyList = []
            this.groupAuthMap = {}
            this.currentGroupIndex = 0
            this.repositoriesGroupCheckedList = []
            this.repositoriesGroupCheckAll = false
            this.$nextTick(() => {
                this.$refs.repositories.init()
            })
        },
        getDetail(id)
        {
            this.spinning = true;
            getPermissionDetail(id).then(res => {
                const { name, privileges, resources } = res
                this.form.name = name;
                this.userAuthMap = {}
                privileges.users.forEach(item => {
                    this.userAuthMap[item.id] = item.access
                })
                if (privileges.users.length)
                    this.getUsers(Object.keys(this.userAuthMap))

                this.groupAuthMap = {}
                privileges.groups.forEach(item => {
                    this.groupAuthMap[item.id] = item.access
                })
                if (privileges.groups.length)
                    this.getGroups(Object.keys(this.groupAuthMap))

                if (resources[0]?.repositoryId) {
                    this.$refs.repositories.step = 0
                    this.$refs.repositories.radioModel = 'Repositories'
                    const repositoryIds = resources.map(item => `${item.storageId}/${item.repositoryId}`)
                    this.$nextTick(() => {
                        this.$refs.repositories.selectedRowKeys = uniq(repositoryIds)

                        if (resources.some(item => item.path)) {
                            this.$nextTick(() => {
                                this.$refs.repositories.setPath(resources)
                            })
                        }
                    })
                } else {
                    this.$refs.repositories.step = 0
                    this.$refs.repositories.radioModel = 'StorageSpace'
                }
            }).finally(() => {
                this.spinning = false;
            })
        },
        getUsers(userIds)
        {
            getPermissionUsers().then(res => {
                res.users.forEach(item => {
                    if (userIds.includes(item.username)) {
                        this.userSelectList.push({
                            key: item.username,
                            title: item.username,
                        })
                    }
                })
                this.currentUserIndex = this.userSelectList[0].key || 0
                this.repositoriesCheckedList = this.userAuthMap[this.currentUserIndex]
            })
        },
        getGroups(groupIds)
        {
            getGroupList({ page: 1, limit: 1000}).then(res => {
                res.data.rows.forEach(item => {
                    if (groupIds.includes(`${item.id}`)) {
                        this.groupSelectList.push({
                            key: `${item.id}`,
                            title: item.groupName,
                        })
                    }
                })
                this.currentGroupIndex = this.groupSelectList[0].key || 0
                this.repositoriesGroupCheckedList = this.groupAuthMap[this.currentGroupIndex]
            }).finally(() => {
                this.loading = false
            })
        },
        onRepositoriesChange(checkedValues)
        {
            this.repositoriesCheckedList = checkedValues;
            this.repositoriesCheckAll = checkedValues.length === 4
            this.userAuthMap[this.currentUserIndex] = checkedValues
        },
        onRepositoriesCheckAllChange(e)
        {
            this.repositoriesCheckAll = e.target.checked;
            this.repositoriesCheckedList = this.repositoriesCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
            this.userAuthMap[this.currentUserIndex] = this.repositoriesCheckedList
        },

        onRepositoriesGroupChange(checkedValues)
        {
            this.repositoriesGroupCheckedList = checkedValues;
            this.repositoriesGroupCheckAll = checkedValues.length === 4
            this.groupAuthMap[this.currentGroupIndex] = checkedValues
        },
        onRepositoriesGroupCheckAllChange(e)
        {
            this.repositoriesGroupCheckAll = e.target.checked;
            this.repositoriesGroupCheckedList = this.repositoriesGroupCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
            this.groupAuthMap[this.currentGroupIndex] = this.repositoriesGroupCheckedList
        },
        openSelectModal(type) {
            const selectedRowKeys = type === 'USER' ? this.userSelectList.map(item => item.key) :
            this.groupSelectList.map(item => item.key)
            this.$refs.selectUserGroup.openModal(type, selectedRowKeys);
        },
        async getStorageList() {
            await new Promise((resolve, reject) => {
                getStorages().then(res => {
                    this.storageList = res.storages;
                    resolve()
                }).catch(e => {
                    reject(e)
                })
            })
        },
        async getRepositoriesList() {
            await new Promise((resolve, reject) => {
                getStoragesAndRepositories().then(res => {
                    this.repositoriesList = []
                    res.forEach(item => {
                        item.children.forEach(ele => {
                            ele.key = ele.key.replace(',','/');
                        })
                        this.repositoriesList.push(...item.children)
                    })
                    resolve()
                }).catch(e => {
                    reject(e)
                })
            })
        },
        selectUserGroupChange(val, type) {
            if (type === 'USER') {
                this.userSelectList = val
                this.userSelectCopyList = val
                this.currentUserIndex = val[0].key || 0
                const userKeys = val.map(item => `${item.key}`)
                for (const key in this.userAuthMap) {
                    if (!userKeys.includes(key)) this.userAuthMap[key] = []
                }
                this.repositoriesCheckedList = this.userAuthMap[this.currentUserIndex]
            } else {
                this.groupSelectList = val
                this.groupSelectCopyList = val
                this.currentGroupIndex = val[0].key || 0
                const groupKeys = val.map(item => `${item.key}`)
                for (const key in this.groupAuthMap) {
                    if (!groupKeys.includes(key)) this.groupAuthMap[key] = []
                }
                this.repositoriesGroupCheckedList = this.groupAuthMap[this.currentGroupIndex]
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
        },
        groupClick(item) {
            this.currentGroupIndex = item.key
            if (this.groupAuthMap[item.key]) {
                this.repositoriesGroupCheckedList = this.groupAuthMap[item.key]
            } else {
                this.groupAuthMap[item.key] = []
                this.repositoriesGroupCheckedList = []
            }
            this.repositoriesGroupCheckAll = this.repositoriesGroupCheckedList.length === 4
        },
        handleConfirm() {
            this.$refs.form.validate(validate => {
                if (validate) {
                    this.confirmLoading = true
                    const groups = this.groupSelectList.map(item => {
                        return {
                            id: item.key,
                            access: this.groupAuthMap[item.key] || []
                        }
                    })
                    const users = this.userSelectList.map(item => {
                        return {
                            id: item.key,
                            access: this.userAuthMap[item.key] || []
                        }
                    })
                    const params = {
                        name: this.form.name,
                        privileges: {
                            groups,
                            users,
                        },
                        resources: this.$refs.repositories.getResources()
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
        }
    }

}
</script>

<style scoped lang="scss">
.step {
    margin-bottom: 20px;

    ::v-deep .ant-steps-item-process .ant-steps-item-icon > .ant-steps-icon{
        color: #FFFFFF;
    }
}

::v-deep .step {
    margin-bottom: 20px;

    .ant-steps-item-process .ant-steps-item-icon > .ant-steps-icon{
        color: #FFFFFF;
    }
}
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
        height: 250px;
        overflow-y: auto;

        .selected-item {
            padding: 8px;
            margin-top: 6px;
            border-radius: 6px;
            cursor: pointer;

            &:hover{
                background: #f1f3f8;
            }
        }

        .active {
            background: rgba(24, 144, 255, 0.1);

            &:hover{
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
</style>