<template>
    <a-drawer
        placement="right"
        width="65%"
        :title="(isEdit ? $t('Permissions.Edit') : $t('Permissions.Create'))"
        :visible="visible"
        @close="closeModal"
    >
        <a-form>
            <a-form-item>
                <a-input v-model="name" :placeholder="$t('Permissions.EnterTheNameCreate')"/>
            </a-form-item>
        </a-form>
        <a-steps v-model="step" type="navigation" size="small" class="step">
            <a-step :title="$t('Permissions.Resources')" :status="step === 0 ? 'process' : 'wait'" :description="$t('Permissions.ResourcesDesc')"/>
            <a-step :title="$t('Permissions.Users')" :status="step === 1 ? 'process' : 'wait'" :description="$t('Permissions.UsersDesc')"/>
            <a-step :title="$t('Permissions.Groups')" :status="step === 2 ? 'process' : 'wait'" :description="$t('Permissions.GroupsDesc')"/>
        </a-steps>
        <div v-if="!step">
            <a-row :gutter="20">
                <a-col :span="6">
                    <div class="resource-item" @click="openChildModal('repositories')">
                        <a-icon :type="repositoriesSelectList.length ? 'edit' : 'plus'" :style="{fontSize: '24px'}"/>
                        <span>{{ $t(`Permissions.${repositoriesSelectList.length ? 'EditRepositories' : 'AddRepositories'}`) }}</span>
                    </div>
                </a-col>
                <a-col :span="6">
                    <div class="resource-item" @click="openChildModal('builds')">
                        <a-icon :type="buildSelectList.length ? 'edit' : 'plus'" :style="{fontSize: '24px'}"/>
                        <span>{{ $t(`Permissions.${buildSelectList.length ? 'EditBuilds' : 'AddBuilds'}`) }}</span>
                    </div>
                </a-col>
                <a-col :span="6">
                    <div class="resource-item" @click="openChildModal('bundles')">
                        <a-icon :type="bundleSelectList.length ? 'edit' : 'plus'" :style="{fontSize: '24px'}"/>
                        <span>{{ $t(`Permissions.${bundleSelectList.length ? 'EditReleaseBundles' : 'AddReleaseBundles'}`) }}</span>
                    </div>
                </a-col>
            </a-row>
        </div>
        <div v-if="step === 1" class="by-flex by-col-stretch">
            <div class="select-content">
                <div class="title">{{ $t(`Permissions.SelectedUser`) }}</div>
                <div class="by-flex by-m-t-10 by-m-b-10">
                    <a-input v-model="userSearch" :placeholder="$t('Permissions.Search')" class="by-w-300"></a-input>
                    <a-button type="primary" icon="edit" class="by-m-l-10" @click="openSelectModal('USER')"/>
                </div>
                <div class="selected-list">
                    <div
                        class="selected-item by-flex by-row-between"
                        v-for="(item, index) in userSelectList"
                        :key="index"
                        :class="{'active': currentUserIndex === index}"
                        @click="currentUserIndex = index"
                    >
                        <span class="by-m-l-10">{{ item.name }}</span>
                        <a-tooltip placement="topLeft" :title="$t('Permissions.NoPermissionsTip')">
                            <a-icon type="exclamation-circle" />
                        </a-tooltip>
                    </div>
                </div>
            </div>
            <div class="by-p-l-32 by-flex-1">
                <div class="permission-item">
                    <div class="title">{{ $t(`Permissions.SelectedUsersRepositories`) }}</div>
                    <div class="by-flex">
                        <a-checkbox-group v-model="repositoriesCheckedList" :options="repositoriesOptions" @change="onRepositoriesChange" />
                        <a-checkbox :checked="repositoriesCheckAll" @change="onRepositoriesCheckAllChange" class="by-m-l-10">
                            {{ $t(`Permissions.SelectAll`) }}
                        </a-checkbox>
                    </div>
                </div>
                <div class="permission-item">
                    <div class="title">{{ $t(`Permissions.SelectedUsersBuilds`) }}</div>
                    <div class="by-flex">
                        <a-checkbox-group v-model="buildCheckedList" :options="repositoriesOptions" @change="onBuildsChange" />
                        <a-checkbox :checked="buildCheckAll" @change="onBuildsCheckAllChange" class="by-m-l-10">
                            {{ $t(`Permissions.SelectAll`) }}
                        </a-checkbox>
                    </div>
                </div>
                <div class="permission-item">
                    <div class="title">{{ $t(`Permissions.SelectedUsersBundles`) }}</div>
                    <div class="by-flex">
                        <a-checkbox-group v-model="bundlesCheckedList" :options="repositoriesOptions" @change="onBundlesChange" />
                        <a-checkbox :checked="bundlesCheckAll" @change="onBundlesCheckAllChange" class="by-m-l-10">
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
                    <a-input v-model="userSearch" :placeholder="$t('Permissions.Search')" class="by-w-300"></a-input>
                    <a-button type="primary" icon="edit" class="by-m-l-10" @click="openSelectModal('GROUP')"/>
                </div>
                <div class="selected-list">
                    <div
                        class="selected-item by-flex by-row-between"
                        v-for="(item, index) in groupSelectList"
                        :key="index"
                        :class="{'active': currentGroupIndex === index}"
                        @click="currentGroupIndex = index"
                    >
                        <span class="by-m-l-10">{{ item.name }}</span>
                        <a-tooltip placement="topLeft" :title="$t('Permissions.GroupNoPermissionsTip')">
                            <a-icon type="exclamation-circle" />
                        </a-tooltip>
                    </div>
                </div>
            </div>
            <div class="by-p-l-32 by-flex-1">
                <div class="permission-item">
                    <div class="title">{{ $t(`Permissions.SelectedGroupRepositories`) }}</div>
                    <div class="by-flex">
                        <a-checkbox-group v-model="repositoriesGroupCheckedList" :options="repositoriesOptions" @change="onRepositoriesGroupChange" />
                        <a-checkbox :checked="repositoriesGroupCheckAll" @change="onRepositoriesGroupCheckAllChange" class="by-m-l-10">
                            {{ $t(`Permissions.SelectAll`) }}
                        </a-checkbox>
                    </div>
                </div>
                <div class="permission-item">
                    <div class="title">{{ $t(`Permissions.SelectedGroupBuilds`) }}</div>
                    <div class="by-flex">
                        <a-checkbox-group v-model="buildGroupCheckedList" :options="repositoriesOptions" @change="onBuildsGroupChange" />
                        <a-checkbox :checked="buildGroupCheckAll" @change="onBuildsGroupCheckAllChange" class="by-m-l-10">
                            {{ $t(`Permissions.SelectAll`) }}
                        </a-checkbox>
                    </div>
                </div>
                <div class="permission-item">
                    <div class="title">{{ $t(`Permissions.SelectedGroupBundles`) }}</div>
                    <div class="by-flex">
                        <a-checkbox-group v-model="bundlesGroupCheckedList" :options="repositoriesOptions" @change="onBundlesGroupChange" />
                        <a-checkbox :checked="bundlesGroupCheckAll" @change="onBundlesGroupCheckAllChange" class="by-m-l-10">
                            {{ $t(`Permissions.SelectAll`) }}
                        </a-checkbox>
                    </div>
                </div>
            </div>
        </div>
        <repositories
            ref="repositories"
            :repositoriesList="repositoriesList"
            :repositoriesSelectList="repositoriesSelectList"
        />
        <builds
            ref="builds"
            :buildList="buildList"
            :buildSelectList="buildSelectList"
        />
        <bundles
            ref="bundles"
            :bundleList="bundleList"
            :bundleSelectList="bundleSelectList"
        />
        <selectUserGroup ref="selectUserGroup"></selectUserGroup>
    </a-drawer>
</template>

<script>
import repositories from "./repositories.vue";
import builds from "./builds.vue";
import bundles from "./bundles.vue";
import selectUserGroup from "./selectUserGroup.vue";

export default {
    name: "modal",
    components: {
        repositories,
        builds,
        bundles,
        selectUserGroup
    },
    data()
    {
        return {
            visible: false,
            isEdit: false,
            step: 0,
            name: '',
            repositoriesList: [],
            repositoriesSelectList: [],
            buildList: [],
            buildSelectList: [],
            bundleList: [],
            bundleSelectList: [],
            userSearch: '',
            userSelectList: [
                {
                    name: 'User 1'
                },
                {
                    name: 'User 2'
                },
                {
                    name: 'User 3'
                },
                {
                    name: 'User 4'
                },
                {
                    name: 'User 2'
                },
                {
                    name: 'User 3'
                },
                {
                    name: 'User 4'
                },
                {
                    name: 'User 2'
                },
                {
                    name: 'User 3'
                },
                {
                    name: 'User 4'
                },
                {
                    name: 'User 5'
                }
            ],
            currentUserIndex: 0,
            repositoriesCheckedList: [],
            repositoriesCheckAll: false,
            buildCheckedList: [],
            buildCheckAll: false,
            bundlesCheckedList: [],
            bundlesCheckAll: false,

            groupSelectList: [
                {
                    name: 'Group 1'
                },
                {
                    name: 'Group 2'
                },
                {
                    name: 'Group 3'
                },
            ],
            currentGroupIndex: 0,
            repositoriesGroupCheckedList: [],
            repositoriesGroupCheckAll: false,
            buildGroupCheckedList: [],
            buildGroupCheckAll: false,
            bundlesGroupCheckedList: [],
            bundlesGroupCheckAll: false,
        }
    },
    computed: {
        repositoriesOptions() {
            return [
                {
                    label: this.$t(`Permissions.Download`),
                    value: 'download'
                },
                {
                    label: this.$t(`Permissions.DeployCache`),
                    value: 'deployCache'
                },
                {
                    label: this.$t(`Permissions.DeleteUpdate`),
                    value: 'deleteUpdate'
                },
            ]
        }
    },
    methods: {
        openModal(isEdit)
        {
            this.visible = true;
            this.isEdit = isEdit;
        },
        closeModal()
        {
            this.visible = false;
        },
        openChildModal(type)
        {
            this.$refs[type].openModal();
        },
        onRepositoriesChange(checkedValues)
        {
            this.repositoriesCheckedList = checkedValues;
            this.repositoriesCheckAll = checkedValues.length === 3
        },
        onRepositoriesCheckAllChange(e)
        {
            this.repositoriesCheckAll = e.target.checked;
            this.repositoriesCheckedList = this.repositoriesCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
        },
        onBuildsChange(checkedValues){
            this.buildCheckedList = checkedValues;
            this.buildCheckAll = checkedValues.length === 3
        },
        onBuildsCheckAllChange(e)
        {
            this.buildCheckAll = e.target.checked;
            this.buildCheckedList = this.buildCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
        },
        onBundlesChange(checkedValues){
            this.bundlesCheckedList = checkedValues;
            this.bundlesCheckAll = checkedValues.length === 3
        },
        onBundlesCheckAllChange(e)
        {
            this.bundlesCheckAll = e.target.checked;
            this.bundlesCheckedList = this.bundlesCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
        },

        onRepositoriesGroupChange(checkedValues)
        {
            this.repositoriesGroupCheckedList = checkedValues;
            this.repositoriesGroupCheckAll = checkedValues.length === 3
        },
        onRepositoriesGroupCheckAllChange(e)
        {
            this.repositoriesGroupCheckAll = e.target.checked;
            this.repositoriesGroupCheckedList = this.repositoriesGroupCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
        },
        onBuildsGroupChange(checkedValues){
            this.buildGroupCheckedList = checkedValues;
            this.buildGroupCheckAll = checkedValues.length === 3
        },
        onBuildsGroupCheckAllChange(e)
        {
            this.buildGroupCheckAll = e.target.checked;
            this.buildGroupCheckedList = this.buildGroupCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
        },
        onBundlesGroupChange(checkedValues){
            this.bundlesGroupCheckedList = checkedValues;
            this.bundlesGroupCheckAll = checkedValues.length === 3
        },
        onBundlesGroupCheckAllChange(e)
        {
            this.bundlesGroupCheckAll = e.target.checked;
            this.bundlesGroupCheckedList = this.bundlesGroupCheckAll ? this.repositoriesOptions.map(item => item.value) : [];
        },
        openSelectModal(type) {
            this.$refs.selectUserGroup.openModal(type);
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