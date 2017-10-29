package fox_goose_bag_of_corn.java;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class TreeNode<T> {

	public T rootVal;
	private List<TreeNode<T>> branches;
	public TreeNode<T> rootParent;

	public TreeNode(T v) {
		rootVal = v;
		branches = new ArrayList<>();
	}

	private TreeNode(TreeNode<T> p, T v) {
		this(v);
		rootParent = p;
	}

	private boolean hasBranches() {
		return !branches.isEmpty();
	}

	private void getLowestBranches(List<TreeNode<T>> acc) {
		if (hasBranches()) {
			for (TreeNode<T> branch: branches) {
				branch.getLowestBranches(acc);
			}
		}
		else {
			acc.add(this);
		}
	}

	public List<TreeNode<T>> getLowestBranches() {
		List<TreeNode<T>> acc = new ArrayList<>();
		getLowestBranches(acc);
		return acc;
	}

	public void addBranches(List<T> branchVals) {
		for (T branchVal: branchVals) {
			branches.add(new TreeNode<>(this, branchVal));
		}
	}

	public boolean isTreeRoot() {
		return rootParent == null;
	}

	public List<T> nodeToValsList() {
		List<T> acc = new ArrayList<>();
		TreeNode<T> node = this;
		while (node != null) {
			acc.add(node.rootVal);
			node = node.rootParent;
		}
		Collections.reverse(acc);
		return acc;
	}

//	public String toPrettyString() {
//		StringBuilder sb = new StringBuilder();
//		sb.append("------------------------------------------");
//		sb.append("\n");
//		toPrettyStringRecursively(this, 1, sb);
//		sb.append("------------------------------------------");
//		sb.append("\n");
//		return sb.toString();
//	}
//
//	private static void toPrettyStringRecursively(TreeNode<?> tree, int level, StringBuilder sb) {
//		sb.append(String.format("Level %d -- %s", level, tree.rootVal));
//		sb.append("\n");
//		for (TreeNode<?> branch: tree.branches) {
//			toPrettyStringRecursively(branch, level + 1, sb);
//		}
//	}
}


