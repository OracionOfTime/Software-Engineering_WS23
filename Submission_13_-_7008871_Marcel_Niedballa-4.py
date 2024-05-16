# This is the python template for Assignment 04.
# - You must use this template.
# - You must not change any signatures of the methods, only edit the sections that are clearly marked for modification
# - The return value of every function has to be in the right format, otherwise this is a desk reject.
# - Plagiarism leads to failing the assignment!

import itertools
import pandas as pd
import typing as tp


def get_name() -> str:
    # TODO: Change this to return your name
    # Write your code here
    return "Marcel Niedballa"


def get_matriculation_number() -> int:
    # TODO: Change this to return your matriculation number
    return 7008871


# Task 1: Create a CART
# Write your helper functions here, if needed
class Node:
    def __init__(self, name, mean, X, Y, configurations, split_by_feature=None, error_of_split=None,
                 successor_left=None,
                 successor_right=None, selected_features: set[str] = None):
        self.name = name
        self.mean = mean
        self.X = X
        self.Y = Y
        self.configurations = configurations
        self.split_by_feature = split_by_feature
        self.error_of_split = error_of_split
        self.successor_left = successor_left
        self.successor_right = successor_right
        self.selected_features = selected_features if selected_features is not None else set()


def yes_branch(x: pd.DataFrame, y: pd.DataFrame, split_by_feature: str):
    selected = x[split_by_feature] == 1
    mean_yes = y[selected].mean()
    error_yes = ((y[selected] - mean_yes) ** 2).sum()

    return x[selected], y[selected], mean_yes, error_yes, split_by_feature, selected


def no_branch(x: pd.DataFrame, y: pd.DataFrame, split_by_feature: str):
    not_selected = x[split_by_feature] != 1
    mean_no = y[not_selected].mean()
    error_no = ((y[not_selected] - mean_no) ** 2).sum()

    return x[not_selected], y[not_selected], mean_no, error_no, split_by_feature, not_selected


def check_split_1(featureSet, node):
    return (featureSet is not None) and (len(node.configurations) > 1)


def check_split_2(yes_part, no_part):
    if len(yes_part[1]) > 0 and len(no_part[1] > 0):
        if yes_part[1].nunique() == 1:
            if no_part[1].nunique() == 1:
                y_val = yes_part[1].values[0]
                n_val = no_part[1].values[0]
                if y_val == n_val:
                    # if yes_part[1].reset_index(drop=True).equals(no_part[1].reset_index(drop=True)):
                    return False
                else:
                    return True
            else:
                return True
        else:
            return True
    else:
        return False


def split(node: Node, sse_min):
    node.split_by_feature = sse_min[1]
    node.error_of_split = sse_min[0]
    chosen_features = node.selected_features | {sse_min[1]}
    yes_values = yes_branch(node.X, node.Y, sse_min[1])
    no_values = no_branch(node.X, node.Y, sse_min[1])

    left_node = Node(f"{node.name}L", yes_values[2],
                     yes_values[0], yes_values[1],
                     node.configurations[yes_values[5]])
    left_node.selected_features = chosen_features
    right_node = Node(f"{node.name}R", no_values[2],
                      no_values[0], no_values[1],
                      node.configurations[no_values[5]])
    right_node.selected_features = chosen_features

    node.successor_left = left_node
    node.successor_right = right_node

    return [left_node, right_node]


def create_dict(node: Node):
    return {
        "name": node.name,
        "mean": node.mean,
        "split_by_feature": node.split_by_feature,
        "error_of_split": node.error_of_split,
        "successor_left": create_dict(node.successor_left) if node.successor_left else None,
        "successor_right": create_dict(node.successor_right) if node.successor_right else None
    }


# End of helper functions
def get_cart(sample_set_csv: str) -> tp.Dict[str, tp.Union[str, float, tp.Dict]]:
    # The sample_set_csv is a file path to a csv data, this can be imported into a dataframe
    df = pd.read_csv(sample_set_csv)
    # TODO: Write your code here. And change the return.
    x: pd.DataFrame = df.iloc[:, 1:-1]
    y: pd.DataFrame = df['performance']
    features = set(x.columns)
    rootNode = Node("X", y.mean(), x, y, df["Id"])
    regTree = [rootNode]

    while regTree:
        currentNode = regTree.pop(0)
        currentFeatures = features - currentNode.selected_features
        possible_feats = []
        if check_split_1(currentFeatures, currentNode):

            for feats in sorted(currentFeatures):
                selected_val = yes_branch(currentNode.X, currentNode.Y, feats)
                deselected_val = no_branch(currentNode.X, currentNode.Y, feats)
                left_error = selected_val[3]
                right_error = deselected_val[3]
                sse_sum = left_error + right_error

                if check_split_2(selected_val, deselected_val):
                    possible_feats.append((sse_sum, feats))
            if not possible_feats:
                continue
            sse_min = min(possible_feats, key=lambda sse: sse[0])
            branches = split(currentNode, sse_min)
            regTree.extend(branches)

    # print(create_dict(rootNode))
    return create_dict(rootNode)


# Task 2a: Predicted performance

# Write your helper functions here, if needed
def none_feature(entry: tp.Dict[str, tp.Union[str, float, tp.Dict]]):
    if entry["split_by_feature"] is None:
        return True
    else:
        return False


def none_succ_left(entry: tp.Dict[str, tp.Union[str, float, tp.Dict]]):
    if entry["successor_left"] is None:
        return True
    else:
        return False


def none_succ_right(entry: tp.Dict[str, tp.Union[str, float, tp.Dict]]):
    if entry["successor_right"] is None:
        return True
    else:
        return False


# End of helper functions
def get_performance(cart: tp.Dict[str, tp.Union[str, float, tp.Dict]], configuration: tp.Set[str]) -> float:
    # TODO: Write your code here. And change the return.
    if none_succ_left(cart) and none_succ_right(cart) and none_feature(cart):
        return cart["mean"]
    else:
        rootFeature = cart["split_by_feature"]

        if rootFeature in configuration:
            configuration.remove(rootFeature)
            return get_performance(cart["successor_left"], configuration)
        else:
            return get_performance(cart["successor_right"], configuration)


# Task 2b: Calculate the error rate
# Write your helper functions here, if needed
def calc_error(cart: tp.Dict[str, tp.Union[str, float, tp.Dict]], df: pd.DataFrame):
    x: pd.DataFrame = df.iloc[:, 1:-1]
    y: pd.DataFrame = df["performance"]
    predicted_val = cart["mean"]
    samples = len(df["Id"])
    differences = []

    for i in range(samples):
        row = x.iloc[i]
        entry = cart
        while not none_feature(entry) and not none_succ_right(entry) and not none_succ_left(entry):
            split_feat = entry["split_by_feature"]
            if split_feat is not None:
                if row[split_feat] == 1:
                    entry = entry["successor_left"]
                else:
                    entry = entry["successor_right"]

                predicted_val = entry["mean"]

        diff = abs(y.iloc[i] - predicted_val)
        differences.append(diff)

    return differences


# End of helper functions
def get_error_rate(cart: tp.Dict[str, tp.Union[str, float, tp.Dict]]
                   , sample_set_csv: str) -> float:
    # The sample_set_csv is a file path to a csv data, this can be imported into a dataframe
    df = pd.read_csv(sample_set_csv)
    rate_list = calc_error(cart, df)
    err_df = pd.Series(rate_list).to_frame()
    err_rate = err_df.mean()
    # TODO: Write your code here. And change the return.
    return err_rate.iloc[0]


# Task 2c: Generate optimal configuration
# Write your helper functions here, if needed
def get_xor_groups(xor_pairs: tp.List[tp.Set[str]], configuration: tp.Set[str]):
    options = list()
    for pair in xor_pairs:
        feat1 = pair.pop()
        feat2 = pair.pop()
        if (feat1 not in configuration) and (feat2 not in configuration):
            options.append({feat1, feat2})

    return options


def check_xor_group(split_feat: str, pairs: tp.List[tp.Set[str]]):
    for pair in pairs:
        if split_feat in pair:
            return True, list(pair)

    return False, None


def calc_path_performance(cart: tp.Dict[str, tp.Union[str, float, tp.Dict]]
                          , partial_config: tp.Set[str]
                          , deselected_pairs: tp.List[tp.Set[str]]
                          , all_feats: tp.Set[str]):
    currentNode = cart
    optimal_path = list()
    remaining_feats = list(all_feats)

    while not none_feature(currentNode) and not none_succ_right(currentNode) and not none_succ_left(currentNode):
        split_feature = currentNode["split_by_feature"]
        check = check_xor_group(split_feature, deselected_pairs)
        if split_feature in partial_config:
            currentNode = currentNode["successor_left"]
            remaining_feats.remove(split_feature)
        elif check[0]:
            index = check[1]
            diff_feat = index[0] if split_feature == index[1] else index[1]
            if currentNode["successor_right"]["mean"] < currentNode["successor_left"]["mean"]:
                currentNode = currentNode["successor_right"]
                optimal_path.append(diff_feat)
                remaining_feats.remove(split_feature)
            else:
                currentNode = currentNode["successor_left"]
                optimal_path.append(split_feature)
                remaining_feats.remove(split_feature)
                remaining_feats.remove(diff_feat)
        else:
            feat_in_all = False
            for feat in remaining_feats:
                if split_feature == feat:
                    if currentNode["successor_left"]["mean"] < currentNode["successor_right"]["mean"]:
                        currentNode = currentNode["successor_left"]
                        optimal_path.append(split_feature)
                        remaining_feats.remove(split_feature)
                        feat_in_all = True
                    else:
                        currentNode = currentNode["successor_right"]
                        remaining_feats.remove(split_feature)
                        feat_in_all = True
            if not feat_in_all:
                currentNode = currentNode["successor_right"]

    return set(optimal_path) | partial_config, currentNode["mean"]


def remove_xors(all_f: tp.Set[str], partial_conf: tp.Set[str], xors: tp.List[tp.Set[str]]):
    for tup in xors:
        pairs = list(tup)
        if pairs[0] in partial_conf or pairs[1] in partial_conf:
            all_f.remove(pairs[0])
            all_f.remove(pairs[1])

    return all_f


# End of helper functions
def get_optimal_configuration(cart: tp.Dict[str, tp.Union[str, float, tp.Dict]]
                              , all_features: tp.Set[str]
                              , partial_configuration: tp.Set[str]
                              , xor_groups: tp.List[tp.Set[str]]) \
        -> tp.Tuple[tp.Set[str], float]:
    # TODO: Write your code here. And change the return.
    left_over_feats = remove_xors(all_features, partial_configuration, xor_groups)
    deselected_xors = get_xor_groups(xor_groups, partial_configuration)
    path_and_performance = calc_path_performance(cart, partial_configuration, deselected_xors, left_over_feats)
    return path_and_performance
